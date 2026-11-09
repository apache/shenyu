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

"""Offline integrity regressions for the HTTP pool benchmark report."""

import copy
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from report import validate_comparison


class ComparisonTest(unittest.TestCase):
    def setUp(self):
        source = Path(__file__).resolve().parents[3] / "docs/performance/http-pool-samples.json"
        samples = json.loads(source.read_text(encoding="utf-8"))
        self.before = samples["before"]
        self.after = samples["after"]

    def test_retained_comparison(self):
        validate_comparison(self.before, self.after)

    def test_shuffled_samples(self):
        self.after["runs"].reverse()
        validate_comparison(self.before, self.after)

    def test_duplicate_sample_hiding_missing_case(self):
        self.after["runs"][1] = copy.deepcopy(self.after["runs"][0])
        with self.assertRaisesRegex(ValueError, "duplicate"):
            validate_comparison(self.before, self.after)

    def test_duplicate_repeat_hiding_missing_repeat(self):
        self.after["runs"][8]["repeat"] = 1
        with self.assertRaisesRegex(ValueError, "duplicate"):
            validate_comparison(self.before, self.after)

    def test_unknown_case(self):
        self.after["runs"][0]["case"] = "unknown"
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_missing_sample(self):
        self.before["runs"].pop()
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_measurement_duration_mismatch(self):
        self.after["measurementSeconds"] *= 2
        with self.assertRaisesRegex(ValueError, "measurementSeconds"):
            validate_comparison(self.before, self.after)

    def test_invalid_settings(self):
        for field in ("repetitions", "measurementSeconds", "warmupSeconds"):
            for value in (0, -1, True, 1.5):
                with self.subTest(field=field, value=value):
                    after = copy.deepcopy(self.after)
                    after[field] = value
                    with self.assertRaises(ValueError):
                        validate_comparison(self.before, after)

    def test_mismatched_workload(self):
        for field, value in (("rate", 1), ("poolType", "ELASTIC"),
                             ("keepAlive", False), ("aiClientControl", True),
                             ("directUpstream", False)):
            with self.subTest(field=field):
                after = copy.deepcopy(self.after)
                after["runs"][0][field] = value
                with self.assertRaisesRegex(ValueError, "workload"):
                    validate_comparison(self.before, after)

    def test_within_arm_workload_mismatch(self):
        self.before["runs"][8]["rate"] += 1
        with self.assertRaisesRegex(ValueError, "workload"):
            validate_comparison(self.before, self.after)

    def test_warmup_mismatch(self):
        self.after["warmupSeconds"] += 1
        with self.assertRaisesRegex(ValueError, "warmupSeconds"):
            validate_comparison(self.before, self.after)

    def test_invalid_repeat_identity(self):
        for value in (True, 1.0):
            with self.subTest(value=value):
                after = copy.deepcopy(self.after)
                after["runs"][0]["repeat"] = value
                with self.assertRaisesRegex(ValueError, "repeat"):
                    validate_comparison(self.before, after)

    def test_intended_queue_capacity_change_is_allowed(self):
        self.assertIsNone(self.before["runs"][0]["pendingAcquireMaxCount"])
        self.assertEqual(4, self.after["runs"][0]["pendingAcquireMaxCount"])
        validate_comparison(self.before, self.after)

    def test_queue_override_cannot_change_between_repetitions(self):
        for arm_name in ("before", "after"):
            with self.subTest(arm=arm_name):
                before, after = copy.deepcopy(self.before), copy.deepcopy(self.after)
                arm = before if arm_name == "before" else after
                arm["runs"][12]["pendingAcquireMaxCount"] = 99
                with self.assertRaisesRegex(ValueError, "pendingAcquireMaxCount"):
                    validate_comparison(before, after)

    def test_queue_override_cannot_change_between_cases(self):
        for sample in self.after["runs"]:
            if sample["case"] == "elastic-overload":
                sample["pendingAcquireMaxCount"] = 8
        with self.assertRaisesRegex(ValueError, "pendingAcquireMaxCount"):
            validate_comparison(self.before, self.after)

    def test_invalid_queue_overrides_are_rejected(self):
        for value in (True, False, 0, -2, 1.5, "4", float("nan"), float("inf")):
            with self.subTest(value=value):
                after = copy.deepcopy(self.after)
                for sample in after["runs"]:
                    sample["pendingAcquireMaxCount"] = value
                with self.assertRaisesRegex(ValueError, "pendingAcquireMaxCount"):
                    validate_comparison(self.before, after)

    def test_missing_queue_override_is_not_an_implicit_default(self):
        del self.after["runs"][0]["pendingAcquireMaxCount"]
        with self.assertRaises((KeyError, ValueError)):
            validate_comparison(self.before, self.after)

    def test_report_describes_actual_queue_overrides(self):
        for value, description in ((8, "8"), (None, "implementation default (no override)"),
                                   (-1, "-1 (unbounded where supported)")):
            with self.subTest(value=value):
                for sample in self.before["runs"]:
                    sample["pendingAcquireMaxCount"] = 16
                for sample in self.after["runs"]:
                    sample["pendingAcquireMaxCount"] = value
                result, output = self.render()
                self.assertEqual(0, result.returncode, result.stderr)
                report = output["http-pool-results.md"]
                self.assertIn("Queue override requested: baseline **16**, candidate **" + description + "**", report)
                self.assertNotIn("explicitly selected queue of 4", report)
                self.assertNotIn("shorter candidate queue", report)

    def test_mixed_queue_overrides_keep_published_artifacts_unchanged(self):
        self.after["runs"][12]["pendingAcquireMaxCount"] = 99
        result, output = self.render(retained=True)
        self.assertNotEqual(0, result.returncode)
        self.assertTrue(all(value == "retained" for value in output.values()))

    def test_missing_measurements_are_not_zero_measurements(self):
        for name in ("http_reqs", "http_req_failed", "successful_requests", "successful_duration"):
            with self.subTest(metric=name):
                after = copy.deepcopy(self.after)
                del after["runs"][0]["summary"]["metrics"][name]
                with self.assertRaises(ValueError):
                    validate_comparison(self.before, after)

    def test_missing_metric_fields_are_rejected(self):
        for name, key in (("http_reqs", "count"), ("http_req_failed", "value"),
                          ("successful_requests", "count"), ("successful_requests", "rate"),
                          ("successful_duration", "p(99)")):
            with self.subTest(metric=name, key=key):
                after = copy.deepcopy(self.after)
                del after["runs"][0]["summary"]["metrics"][name][key]
                with self.assertRaises(ValueError):
                    validate_comparison(self.before, after)

    def test_invalid_measurements_are_rejected(self):
        for name, key in (("http_reqs", "count"), ("http_req_failed", "value"),
                          ("successful_requests", "count"), ("successful_requests", "rate"),
                          ("successful_duration", "p(99)")):
            for value in (None, True, "0", -1, float("nan"), float("inf")):
                with self.subTest(metric=name, key=key, value=value):
                    after = copy.deepcopy(self.after)
                    after["runs"][0]["summary"]["metrics"][name][key] = value
                    with self.assertRaises(ValueError):
                        validate_comparison(self.before, after)

    def test_error_rate_above_one_is_rejected(self):
        self.after["runs"][0]["summary"]["metrics"]["http_req_failed"]["value"] = 1.01
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_no_requests_is_not_a_measured_run(self):
        self.after["runs"][0]["summary"]["metrics"]["http_reqs"]["count"] = 0
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_success_count_agrees_with_retained_latency_samples(self):
        self.after["runs"][0]["successfulLatencyMillis"].pop()
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_invalid_latency_samples_are_rejected(self):
        for value in (None, True, "0", -1, float("nan"), float("inf")):
            with self.subTest(value=value):
                after = copy.deepcopy(self.after)
                after["runs"][0]["successfulLatencyMillis"][0] = value
                with self.assertRaises(ValueError):
                    validate_comparison(self.before, after)

    def test_zero_successes_need_independent_counter_evidence(self):
        self.all_failed(self.after["runs"][0])
        validate_comparison(self.before, self.after)
        del self.after["runs"][0]["summary"]["metrics"]["failed_requests"]
        with self.assertRaises(ValueError):
            validate_comparison(self.before, self.after)

    def test_zero_successes_have_unavailable_latency_not_zero(self):
        # One unmeasurable repetition must not silently reduce the median's sample set.
        self.all_failed(self.after["runs"][0])
        result, output = self.render()
        self.assertEqual(0, result.returncode, result.stderr)
        lines = output["http-pool-results.md"].splitlines()
        comparison = next(line for line in lines if line.startswith("| direct-slow-control | 160 |"))
        self.assertIn(" → N/A |", comparison)
        variability = next(line for line in lines if line.startswith("| direct-slow-control |") and "–" in line)
        self.assertTrue(variability.endswith("| N/A |"), variability)

    def test_failed_validation_keeps_published_artifacts_unchanged(self):
        self.after["runs"][0]["summary"]["metrics"]["http_req_failed"]["value"] = None
        result, output = self.render(retained=True)
        self.assertNotEqual(0, result.returncode)
        self.assertTrue(all(value == "retained" for value in output.values()))

    def test_failed_render_keeps_published_artifacts_unchanged(self):
        del self.before["build"]["commit"]
        result, output = self.render(retained=True)
        self.assertNotEqual(0, result.returncode)
        self.assertTrue(all(value == "retained" for value in output.values()))

    @staticmethod
    def all_failed(run):
        metrics = run["summary"]["metrics"]
        total = metrics["http_reqs"]["count"]
        metrics.pop("successful_requests", None)
        metrics.pop("successful_duration", None)
        metrics["failed_requests"] = {"count": total, "rate": metrics["http_reqs"]["rate"]}
        metrics["http_req_failed"] = {"value": 1, "passes": total, "fails": 0}
        run["successfulLatencyMillis"] = []

    def render(self, retained=False):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / "report"
            names = ("http-pool-samples.json", "http-pool-results.md")
            if retained:
                output.mkdir()
                for name in names:
                    (output / name).write_text("retained", encoding="utf-8")
            for name, arm in (("before", self.before), ("after", self.after)):
                (root / (name + ".json")).write_text(json.dumps(arm), encoding="utf-8")
            result = subprocess.run([sys.executable, str(Path(__file__).with_name("report.py")),
                                     "--before", str(root / "before.json"), "--after", str(root / "after.json"),
                                     "--output", str(output)], text=True, capture_output=True, check=False)
            contents = {name: (output / name).read_text(encoding="utf-8") for name in names if (output / name).is_file()}
            return result, contents


if __name__ == "__main__":
    unittest.main()

