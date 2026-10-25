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

"""Exercise the upstream over HTTP and validate every consuming Compose file."""

import http.client
import json
from pathlib import Path
import subprocess
import threading
import unittest

from httpbin import HttpbinHandler, ThreadingHTTPServer


class HttpbinTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = ThreadingHTTPServer(("127.0.0.1", 0), HttpbinHandler)
        cls.thread = threading.Thread(target=cls.server.serve_forever)
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()
        cls.thread.join()

    def setUp(self):
        self.connection = http.client.HTTPConnection(*self.server.server_address, timeout=3)

    def tearDown(self):
        self.connection.close()

    def test_echo_methods_paths_and_request_bodies(self):
        for method in ("GET", "POST", "PUT", "DELETE"):
            for path in ("/anything", "/anything/xx", "/anything/xx/yy", "/anything/xxx"):
                with self.subTest(method=method, path=path):
                    self.connection.request(method, path + "?id=23", body="request body")
                    response = self.connection.getresponse()
                    self.assertEqual(200, response.status)
                    body = json.loads(response.read())
                    self.assertEqual(method, body["method"])
                    self.assertEqual(path + "?id=23", body["url"])
                    self.assertEqual({"id": ["23"]}, body["args"])
                    self.assertEqual("request body", body["data"])

    def test_status_200_has_empty_body_for_every_method(self):
        for method in ("GET", "POST", "PUT", "DELETE", "HEAD"):
            with self.subTest(method=method):
                self.connection.request(method, "/status/200")
                response = self.connection.getresponse()
                self.assertEqual(200, response.status)
                self.assertEqual(b"", response.read())

    def test_unknown_path_is_not_a_success(self):
        self.connection.request("GET", "/not-a-fixture")
        response = self.connection.getresponse()
        self.assertEqual(404, response.status)
        response.read()

    def test_compose_uses_healthy_local_upstream(self):
        directory = Path(__file__).resolve().parent
        compose = directory.parent
        files = sorted((compose / "storage").glob("shenyu-storage-*.yml"))
        files += sorted((compose / "sync").glob("shenyu-sync-websocket-cluster-*.yml"))
        self.assertEqual(6, len(files))
        for path in files:
            with self.subTest(compose=path.name):
                result = subprocess.run(
                    ["docker", "compose", "-f", str(path), "config", "--format", "json"],
                    check=True, capture_output=True, text=True,
                )
                services = json.loads(result.stdout)["services"]
                fixture = services["httpbin"]
                self.assertIn("httpbin.org", fixture["networks"]["shenyu"]["aliases"])
                self.assertEqual("service_healthy", services["shenyu-bootstrap"]["depends_on"]["httpbin"]["condition"])
                mount = next(v for v in fixture["volumes"] if v["target"] == "/opt/httpbin.py")
                self.assertEqual(directory / "httpbin.py", Path(mount["source"]))
                self.assertTrue(mount["read_only"])
                self.assertIn("/anything", fixture["healthcheck"]["test"][-1])


if __name__ == "__main__":
    unittest.main()
