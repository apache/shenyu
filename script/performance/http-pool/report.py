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

"""Render repeat medians from the retained connection-pool benchmark samples."""

import argparse
import json
from pathlib import Path
import statistics


def metric(run, name, key, default=0):
    return run["summary"]["metrics"].get(name, {}).get(key, default)


def median(runs, function):
    return statistics.median(function(run) for run in runs)


def p99(run):
    return metric(run, "successful_duration", "p(99)")


def errors(run):
    return 100 * metric(run, "http_req_failed", "value")


def resource(run, key):
    return run["resources"][key]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--before", type=Path, required=True)
    parser.add_argument("--after", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    before = json.loads(args.before.read_text(encoding="utf-8"))
    after = json.loads(args.after.read_text(encoding="utf-8"))
    for arm in (before, after):
        if len(arm["runs"]) != 8 * arm["repetitions"]:
            raise SystemExit("Incomplete benchmark arm")
        if "productionDiffSha256" in arm["build"]:
            arm["build"]["trackedProductionDiffSha256"] = arm["build"].pop("productionDiffSha256")
    cases = list(dict.fromkeys(run["case"] for run in before["runs"]))
    verified_runtime = " (also verified in both JFR recordings)" if before.get("jfrRuntimeVersion") and after.get("jfrRuntimeVersion") else ""
    groups = {case: ([run for run in before["runs"] if run["case"] == case],
                     [run for run in after["runs"] if run["case"] == case]) for case in cases}
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "http-pool-samples.json").write_text(json.dumps({"before": before, "after": after}, separators=(",", ":")) + "\n", encoding="utf-8")
    lines = [
        "# HTTP pool capacity experiment — 2026-10-15", "",
        "This is a controlled HTTP/1.1 client-pool experiment for #7644, not a full gateway throughput claim.",
        "The required resource bound is the reason for the change; no maximum sustainable QPS or universal latency improvement is claimed.", "",
        "See [policy, migration and reproduction](http-connection-pools.md) and the retained",
        "[per-run summaries, successful latency samples and queue lifetimes](http-pool-samples.json).", "",
        "## Contract", "",
        "- Reviewed source baseline: `" + before["build"]["commit"] + "`.",
        "- Baseline uses the original factory bytecode. The fixture/property accessor and optional Micrometer dependency were added before snapshotting, without changing that factory's policy.",
        "- Candidate uses the changed factory. Baseline: implicit FIXED queue `2 * 8 = 16`, legacy unlimited ELASTIC. Candidate: 8 connections and an explicitly selected queue of 4 for both types, 250 ms acquisition timeout.",
        "- Keep-alive is an explicit test variable; its production default is unchanged. Both arms have metrics enabled. The AI client control uses the unchanged `HttpClient.create()` construction path, not the HTTP factory's provider or a complete AI engine.",
        "- Each case has a fresh JVM, " + str(before["warmupSeconds"]) + " s warmup and " + str(before["measurementSeconds"]) + " s fixed-arrival measurement, repeated three times. All figures below are medians; individual samples and variability are retained.",
        "- Benchmark runtime: `" + before["javaVersion"].splitlines()[0] + "`" + verified_runtime + ". Build/test JDK: `" + str(before["build"]["java"]) + "`; OS: `" + before["build"]["os"] + "`; host logical CPUs: " + str(before["build"]["hostLogicalCpus"]) + ".",
        "- JVM flags: `" + " ".join(before["build"]["jvmFlags"]) + "`. Two worker threads each for upstream, forwarder and client. The processor hint is not an OS CPU quota.",
        "- Generator, forwarder and upstream share one desktop. Background desktop activity is not controlled. No builds or other benchmark arms ran concurrently with either retained arm.",
        "- Upstream: `/fast` returns a small `ok` body; `/slow` delays 100 ms; `/stream` sends ten SSE chunks over 1 s. The direct slow-upstream control runs at the same 160/s offered overload rate.",
        "- Fixture JAR SHA-256 (baseline): `" + before["build"]["fixtureSha256"] + "`.",
        "- Fixture JAR SHA-256 (candidate): `" + after["build"]["fixtureSha256"] + "`. JAR snapshots include production and fixture classes; the tracked-source diff hash alone does not cover new untracked files.",
        "- k6: `" + before["k6Version"] + "`.", "",
        "## Successful traffic and latency", "",
        "k6 rates include completion/drain time. Error percentage includes rejected overload requests; successful-only p99 prevents fast failures from making the latency comparison look better.", "",
        "| Case | Offered/s | Successful/s before → after | Successful p99 ms before → after | Error % before → after |",
        "| --- | ---: | ---: | ---: | ---: |",
    ]
    for case, (old, new) in groups.items():
        rates = [median(runs, lambda run: metric(run, "successful_requests", "rate")) for runs in (old, new)]
        tails = [median(runs, p99) for runs in (old, new)]
        errs = [median(runs, errors) for runs in (old, new)]
        lines.append(f"| {case} | {old[0]['rate']} | {rates[0]:.2f} → {rates[1]:.2f} | {tails[0]:.2f} → {tails[1]:.2f} | {errs[0]:.2f} → {errs[1]:.2f} |")
    lines += ["", "## Resource peaks", "", "Peaks are sampled every 20 ms and then summarized across repetitions. Idle reusable connections are included in open physical upstream connections. New connections are counted at physical channel initialization, not per HTTP request.", "",
              "| Case | Open upstream connections before → after | Pending before → after | New connections/s before → after | Heap MiB before → after | Direct MiB before → after | Windows process handles before → after |",
              "| --- | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for case, (old, new) in groups.items():
        def pair(function, decimals=0):
            a, b = [median(runs, function) for runs in (old, new)]
            return f"{a:.{decimals}f} → {b:.{decimals}f}"
        pending = "not instrumented" if old[0]["aiClientControl"] or old[0]["directUpstream"] else pair(lambda run: resource(run, "peakPending"))
        lines.append("| " + case + " | " + pair(lambda run: resource(run, "peakOpenUpstreamConnections")) + " | " + pending + " | "
                     + pair(lambda run: resource(run, "newConnections") / before["measurementSeconds"], 2) + " | "
                     + pair(lambda run: resource(run, "peakHeapBytes") / 1048576, 2) + " | "
                     + pair(lambda run: resource(run, "peakDirectBytes") / 1048576, 2) + " | "
                     + pair(lambda run: resource(run, "peakWindowsProcessHandles")) + " |")
    lines += ["", "## Variability and interpretation", "",
              "| Case | Successful p99 range before (ms) | Successful p99 range after (ms) |",
              "| --- | ---: | ---: |"]
    for case, (old, new) in groups.items():
        lines.append(f"| {case} | {min(map(p99, old)):.2f}–{max(map(p99, old)):.2f} | {min(map(p99, new)):.2f}–{max(map(p99, new)):.2f} |")
    drops = sum(metric(run, "dropped_iterations", "count") for arm in (before, after) for run in arm["runs"])
    lines += ["", "- Total generator dropped iterations: " + str(drops) + ".",
              "- The unchanged FIXED default is not being presented as an unbounded queue: the before measurement exposes its existing 16-slot limit. The shorter candidate queue is an explicit configuration choice.",
              "- ELASTIC shedding under the new 8-connection budget is intentional admission control, not a throughput improvement. Before ELASTIC can complete more requests by consuming more upstream connections.",
              "- The direct control and AI-construction stream control prevent attributing upstream or generator limits to the new HTTP pool. The AI control is not an end-to-end AI transport or model benchmark.",
              "- Pending lifetimes in the raw data measure queue residence, including timeout/cancellation; the production acquisition timer additionally includes connection establishment.",
              "- Heap peaks are sampled live heap, not retained-heap/leak measurements. Direct-buffer peaks and open connections are separate quantities. This short run cannot establish long-term memory stability.",
              "- Windows has no POSIX FD counter here (`peakFileDescriptors: -1`). Native process-handle counts are retained separately. Linux FD validation remains an explicit follow-up; these handle counts must not be represented as FD measurements.",
              "- JFR `profile` recordings for the first fixed-overload repetition of each arm and complete k6 point streams remain in the runner output directories. They are local diagnostic artifacts; the checked-in JSON retains primary latency/queue samples, summaries and resource counters.",
              "- Longer isolated runs, a stated p99/error SLO rate sweep, separate generator/upstream hosts, Linux FD measurement and full gateway/TLS/AI-engine validation remain necessary before deployment sizing or maximum-throughput claims. These are not claimed complete by this local report.", ""]
    (args.output / "http-pool-results.md").write_text("\n".join(lines), encoding="utf-8")
    print(args.output / "http-pool-results.md")


if __name__ == "__main__":
    main()
