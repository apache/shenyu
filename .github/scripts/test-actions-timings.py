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

"""Test the timing partition against completed, skipped and incomplete job records."""

import importlib.util
import unittest
from pathlib import Path
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("timings", Path(__file__).with_name("actions-timings.py"))
TIMINGS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TIMINGS)


class TimingsTest(unittest.TestCase):
    def setUp(self):
        self.run = dict(id=1, run_attempt=1, name="it", event="pull_request", head_sha="abc",
                        created_at="2026-11-01T00:00:00Z")
        self.job = dict(id=2, name="build (http)", status="completed", conclusion="success", html_url="url",
                        created_at="2026-11-01T00:00:20Z", started_at="2026-11-01T00:00:22Z",
                        completed_at="2026-11-01T00:05:30Z", steps=[
                            dict(name="Build with Maven", conclusion="success", started_at="2026-11-01T00:00:30Z",
                                 completed_at="2026-11-01T00:04:00Z"),
                            dict(name="Run test", conclusion="success", started_at="2026-11-01T00:05:00Z",
                                 completed_at="2026-11-01T00:05:20Z")])

    def test_preparation_is_not_reported_as_tests(self):
        result = TIMINGS.row(self.run, self.job, "apache/shenyu")
        self.assertEqual((result["queue_seconds"], result["runner_queue_seconds"],
                          result["preparation_seconds"], result["verification_seconds"]), (22, 2, 288, 20))

    def test_pr_maven_test_step_is_verification(self):
        self.run["name"] = "ci"
        self.job["name"] = "pr_build"
        self.assertEqual(TIMINGS.row(self.run, self.job, "apache/shenyu")["verification_seconds"], 230)

    def test_skipped_jobs_have_no_queue_or_work(self):
        self.job.update(conclusion="skipped", started_at=None, completed_at=None, steps=[])
        result = TIMINGS.row(self.run, self.job, "apache/shenyu")
        self.assertEqual(result["queue_seconds"] + result["preparation_seconds"] + result["verification_seconds"], 0)

    def test_incomplete_timestamps_are_not_negative(self):
        self.assertEqual(TIMINGS.seconds(None, "2026-11-01T00:00:00Z"), 0)
        self.assertEqual(TIMINGS.seconds("2026-11-01T00:01:00Z", "2026-11-01T00:00:00Z"), 0)

    def test_all_jobs_are_paginated(self):
        response = dict(jobs=[self.job], total_count=2)
        with patch.object(TIMINGS, "api", side_effect=[response, response]) as api:
            self.assertEqual(len(TIMINGS.collect("apache/shenyu", self.run)), 2)
            self.assertTrue(api.call_args.args[0].endswith("page=2"))


if __name__ == "__main__":
    unittest.main()
