#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Guard the protocol fixture and runtime boundary independently of Maven caches."""

import json
import tempfile
import time
import unittest
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

from websockets.sync.client import connect

from benchmark import error_code, load
from fixture import SyncFixture, configuration
from inventory import PLUGINS, inventory


class ProtocolTest(unittest.TestCase):
    def test_snapshot_framing_and_incremental_deletion(self):
        fixture = SyncFixture(12345)
        try:
            with connect(f"ws://127.0.0.1:{fixture.port}/websocket") as client:
                client.send("MYSELF_V1:test-attempt")
                frames = [json.loads(client.recv(timeout=5)) for _ in range(5)]
                self.assertEqual([0, 1, 2, 3, 4], [frame["sequence"] for frame in frames])
                self.assertTrue(all(frame["requestId"] == "test-attempt" for frame in frames))
                self.assertNotIn("payload", frames[-1])
                payloads = [json.loads(frame["payload"]) for frame in frames[:-1]]
                self.assertEqual([True, True, True, False], [data["fullSnapshot"] for data in payloads])
                deadline = time.monotonic() + 5
                while not fixture.connections and time.monotonic() < deadline:
                    time.sleep(0.01)
                fixture.update("RULE", fixture.data["RULE"], "DELETE")
                deletion = json.loads(client.recv(timeout=5))
                self.assertEqual("DELETE", deletion["eventType"])
                self.assertFalse(deletion["fullSnapshot"])
                self.assertEqual([], fixture.data["RULE"])
        finally:
            fixture.close()

    def test_recreation_allocates_a_new_selector_identity(self):
        fixture = SyncFixture(12345)
        try:
            previous = fixture.data["SELECTOR"][0]["id"]
            fixture.update("SELECTOR", fixture.data["SELECTOR"], "DELETE")
            fixture.reset(12346)
            current = fixture.data["SELECTOR"][0]["id"]
            self.assertNotEqual(previous, current)
            self.assertEqual(current, fixture.data["RULE"][0]["selectorId"])
            self.assertEqual(current, fixture.upstreams(12347)[0]["selectorId"])
        finally:
            fixture.close()

    def test_backend_success_and_unrelated_errors_do_not_prove_deletion(self):
        self.assertFalse(error_code(b"upstream-a:ok", -102))
        self.assertFalse(error_code(b'{"code":-119}', -102))
        self.assertTrue(error_code(b'{"code":-102}', -102))


class LoadResultTest(unittest.TestCase):
    @staticmethod
    def summary():
        return {"state": {"testRunDurationMs": 25000}, "metrics": {
            "http_reqs": {"values": {"count": 2000, "rate": 80}},
            "successful_requests": {"values": {"count": 1800, "rate": 72}},
            "http_req_duration": {"values": {"p(99)": 200}},
        }}

    def run_load(self, summary, returncode=99):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / "load"
            def start(*args, **kwargs):
                if summary is not None:
                    Path(kwargs["env"]["SUMMARY"]).write_text(json.dumps(summary), encoding="utf-8")
                return Mock(returncode=returncode, poll=Mock(return_value=returncode))
            with patch("benchmark.subprocess.Popen", side_effect=start):
                return load(SimpleNamespace(k6="k6", p99_ms=100), "http://127.0.0.1/bench/ok",
                            b"upstream-a", 100, 20, output)

    def test_throughput_includes_graceful_stop_time(self):
        result = self.run_load(self.summary())
        self.assertEqual(72, result["successful_rps"])
        self.assertEqual(25, result["measurement_duration_s"])
        self.assertEqual(20, result["offered_duration_s"])
        self.assertEqual(100, result["offered_rps"])
        self.assertEqual(2000, result["requests"])
        self.assertEqual(200, result["errors"])
        self.assertEqual(0, result["dropped_iterations"])
        self.assertFalse(result["thresholds_passed"])

    def test_successful_run_preserves_the_threshold_result(self):
        summary = self.summary()
        summary["metrics"]["successful_requests"]["values"] = {"count": 2000, "rate": 80}
        summary["metrics"]["http_req_duration"]["values"]["p(99)"] = 80
        result = self.run_load(summary, 0)
        self.assertTrue(result["thresholds_passed"])
        self.assertEqual(80, result["successful_rps"])
        self.assertEqual(0, result["errors"])

    def test_threshold_failure_remains_a_valid_sweep_point(self):
        summary = self.summary()
        summary["metrics"]["dropped_iterations"] = {"values": {"count": 10}}
        result = self.run_load(summary, 99)
        self.assertFalse(result["thresholds_passed"])
        self.assertEqual(10, result["dropped_iterations"])
        self.assertEqual(72, result["successful_rps"])

    def test_zero_successes_is_valid_when_requests_completed(self):
        summary = self.summary()
        summary["metrics"]["successful_requests"]["values"] = {"count": 0, "rate": 0}
        result = self.run_load(summary, 99)
        self.assertFalse(result["thresholds_passed"])
        self.assertEqual(0, result["successful_rps"])
        self.assertEqual(2000, result["errors"])

    def test_generator_failures_are_not_saturation_evidence(self):
        for code in (1, 103, 104, 105, 107, 108, 110, -9):
            with self.subTest(returncode=code), self.assertRaisesRegex(RuntimeError, "k6.*exit.*k6.log"):
                self.run_load(self.summary(), code)

    def test_generator_failure_without_summary_reports_exit_code(self):
        with self.assertRaisesRegex(RuntimeError, "k6.*104.*k6.log"):
            self.run_load(None, 104)

    def test_missing_counter_is_not_silently_treated_as_zero(self):
        for name in ("http_reqs", "successful_requests"):
            summary = self.summary()
            del summary["metrics"][name]
            with self.subTest(metric=name), self.assertRaisesRegex(ValueError, "k6 summary"):
                self.run_load(summary)

    def test_invalid_measurement_duration_is_rejected(self):
        for duration in (0, -1, float("nan"), float("inf"), None, True, "25000"):
            summary = self.summary()
            summary["state"]["testRunDurationMs"] = duration
            with self.subTest(duration=duration), self.assertRaisesRegex(ValueError, "k6 summary"):
                self.run_load(summary)

    def test_missing_measurement_duration_is_rejected(self):
        summary = self.summary()
        del summary["state"]
        with self.assertRaisesRegex(ValueError, "k6 summary"):
            self.run_load(summary)

    def test_invalid_dropped_iteration_count_is_rejected(self):
        for dropped in (-1, 0.5, float("inf"), None, True):
            summary = self.summary()
            summary["metrics"]["dropped_iterations"] = {"values": {"count": dropped}}
            with self.subTest(dropped=dropped), self.assertRaisesRegex(ValueError, "k6 summary"):
                self.run_load(summary)

    def test_impossible_request_counts_are_rejected(self):
        for requests, successes in ((0, 0), (-1, 0), (2000, -1), (2000, 2001), (2000, float("nan")), (0.5, 0), (True, 0)):
            summary = self.summary()
            summary["metrics"]["http_reqs"]["values"]["count"] = requests
            summary["metrics"]["successful_requests"]["values"]["count"] = successes
            with self.subTest(requests=requests, successes=successes), self.assertRaisesRegex(ValueError, "k6 summary"):
                self.run_load(summary)


class InventoryTest(unittest.TestCase):
    @staticmethod
    def jar(directory, artifact):
        with zipfile.ZipFile(directory / "lib" / f"{artifact}-1.jar", "w") as archive:
            archive.writestr(f"META-INF/maven/org.apache.shenyu/{artifact}/pom.properties",
                             f"groupId=org.apache.shenyu\nartifactId={artifact}\nversion=1\n")
            archive.writestr("META-INF/LICENSE", "Apache-2.0")

    def test_runtime_provider_leaks_are_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            directory = Path(temp)
            for name in ("lib", "conf", "licenses"):
                (directory / name).mkdir()
            for name in ("LICENSE", "NOTICE", "conf/application.yml"):
                (directory / name).write_text("fixture", encoding="utf-8")
            for artifact in PLUGINS | {"shenyu-sync-data-api", "shenyu-sync-data-websocket"}:
                self.jar(directory, artifact)
            result = inventory(directory, minimal=True)
            self.assertEqual(7, result["jar_count"])
            self.assertTrue(all(jar["sha256"] and jar["embedded_legal_files"] for jar in result["jars"]))
            self.jar(directory, "shenyu-sync-data-etcd")
            with self.assertRaisesRegex(ValueError, "synchronization inventory"):
                inventory(directory, minimal=True)
            (directory / "lib" / "shenyu-sync-data-etcd-1.jar").unlink()
            self.jar(directory, "shenyu-registry-zookeeper")
            with self.assertRaisesRegex(ValueError, "Registry implementation"):
                inventory(directory, minimal=True)

    def test_assembly_stays_out_of_the_default_reactor(self):
        root = Path(__file__).resolve().parents[3]
        namespace = {"m": "http://maven.apache.org/POM/4.0.0"}
        pom = ET.parse(root / "pom.xml").getroot()
        self.assertNotIn("shenyu-bootstrap-http", [node.text for node in pom.findall("m:modules/m:module", namespace)])
        profile = pom.find("m:profiles/m:profile[m:id='minimal-http']", namespace)
        self.assertEqual("shenyu-bootstrap-http", profile.find("m:modules/m:module", namespace).text)


if __name__ == "__main__":
    unittest.main()
