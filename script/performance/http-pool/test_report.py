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


if __name__ == "__main__":
    unittest.main()
