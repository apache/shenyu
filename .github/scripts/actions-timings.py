#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Export per-job queue, preparation and verification seconds using the GitHub API.

Usage: python3 .github/scripts/actions-timings.py --repo apache/shenyu --limit 50 > timings.csv
Queue includes dependency wait; runner_queue_seconds shows the API-observable runner wait.
Verification is the duration of test/check steps, including compilation within those steps.
Image builds, tool installation, deployment, cache handling and post steps are preparation.
Failed, skipped and cancelled runs remain visible and must not be averaged with successful runs.
"""

import argparse
import concurrent.futures
import csv
import datetime
import json
import re
import subprocess
import sys
import time

VERIFY = re.compile(
    r"^(?:Run (?:test|Maven tests|E2E Tests|Java 21 compatibility tests|Apache RAT)|"
    r"Build with Maven$|Replay registration contract|Check resolved registry|"
    r"Check Agent|Check License|Check WAT|Verify list/detail|Run native fresh|"
    r"Check harness|Build disposable Admin|Perform CodeQL|Test |Check test result|"
    r"Cluster Test|Require every|checking job status|python3 .*test-)", re.IGNORECASE)


def api(path):
    """Retry transient API failures once, respecting the repository's backoff policy."""
    for attempt in range(2):
        result = subprocess.run(["gh", "api", path], text=True, capture_output=True)
        if result.returncode == 0:
            return json.loads(result.stdout)
        if attempt == 0 and re.search(r"429|5\d\d|timeout|timed out", result.stderr, re.IGNORECASE):
            time.sleep(20 if "429" in result.stderr else 2)
            continue
        raise RuntimeError(result.stderr.strip())


def seconds(start, end):
    """Incomplete/skipped timestamps have no measurable duration."""
    if not start or not end:
        return 0
    return max(0, (datetime.datetime.fromisoformat(end.replace("Z", "+00:00"))
                   - datetime.datetime.fromisoformat(start.replace("Z", "+00:00"))).total_seconds())


def row(run, job, repo):
    """Partition the job's lifetime, retaining skipped jobs as zero-duration rows."""
    elapsed = seconds(job.get("started_at"), job.get("completed_at"))
    verification = 0
    for step in job.get("steps", []):
        if step.get("conclusion") == "skipped":
            continue
        name = step["name"]
        # Build-only jobs use this same label; their tests are disabled explicitly.
        if name == "Build with Maven" and not (run["name"] == "ci" and job["name"] == "pr_build"):
            continue
        if VERIFY.search(name):
            verification += seconds(step.get("started_at"), step.get("completed_at"))
    skipped = job.get("conclusion") == "skipped"
    queue = 0 if skipped else seconds(run["created_at"], job.get("started_at"))
    runner_queue = 0 if skipped else seconds(job.get("created_at"), job.get("started_at"))
    return {
        "repository": repo, "run_id": run["id"], "attempt": run["run_attempt"],
        "workflow": run["name"], "event": run["event"], "sha": run["head_sha"],
        "job_id": job["id"], "job": job["name"], "status": job["status"],
        "conclusion": job.get("conclusion"), "queue_seconds": queue,
        "runner_queue_seconds": runner_queue, "preparation_seconds": max(0, elapsed - verification),
        "verification_seconds": verification, "url": job["html_url"],
    }


def collect(repo, run):
    jobs = []
    page = 1
    while True:
        response = api(f"repos/{repo}/actions/runs/{run['id']}/jobs?per_page=100&page={page}")
        jobs.extend(response["jobs"])
        if len(jobs) >= response["total_count"]:
            return [row(run, job, repo) for job in jobs]
        page += 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default="apache/shenyu")
    parser.add_argument("--limit", type=int, default=50, choices=range(1, 101))
    parser.add_argument("--run", action="append", type=int, help="Inspect specific run IDs instead of recent runs")
    args = parser.parse_args()
    if args.run:
        runs = [api(f"repos/{args.repo}/actions/runs/{run_id}") for run_id in args.run]
    else:
        runs = api(f"repos/{args.repo}/actions/runs?per_page={args.limit}")["workflow_runs"]
    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
        results = list(pool.map(lambda run: collect(args.repo, run), runs))
    rows = [item for group in results for item in group]
    if rows:
        writer = csv.DictWriter(sys.stdout, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
