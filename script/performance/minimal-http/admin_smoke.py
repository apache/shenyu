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

"""Run a disposable H2 Admin and packaged HTTP gateway; mutate routes through REST."""

import argparse
import json
import os
import secrets
import shutil
import subprocess
import time
from pathlib import Path
from types import SimpleNamespace
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from benchmark import error_code, free_port, launch, stop, wait_for
from fixture import NAMESPACE, Upstream, condition


class Admin:
    def __init__(self, url):
        self.url, self.token = url, None

    def call(self, method, path, body=None):
        headers = {"Content-Type": "application/json"}
        if self.token:
            headers["X-Access-Token"] = self.token
        payload = json.dumps(body).encode() if body is not None else None
        with urlopen(Request(self.url + path, data=payload, headers=headers, method=method), timeout=5) as response:
            raw = response.read().decode()
        if raw == "success":
            return raw
        value = json.loads(raw)
        if value.get("code") != 200:
            raise RuntimeError(f"Admin {method} {path}: {value.get('message')}")
        return value.get("data")

    def login(self):
        data = self.call("POST", "/platform/login", {"userName": "admin", "password": "123456"})
        self.token = data["token"]

    def rows(self, resource, **params):
        query = urlencode(dict(currentPage=1, pageSize=100, namespaceId=NAMESPACE, **params))
        data = self.call("GET", f"/{resource}?{query}")
        return data.get("dataList", data.get("list", []))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--admin", required=True, type=lambda s: Path(s).resolve())
    parser.add_argument("--minimal", required=True, type=lambda s: Path(s).resolve())
    parser.add_argument("--output", required=True, type=lambda s: Path(s).resolve())
    parser.add_argument("--java", default=shutil.which("java"))
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    port = free_port()
    sync_token = secrets.token_urlsafe(32)
    log = (args.output / "admin.log").open("w", encoding="utf-8")
    command = [args.java, "-Xms512m", "-Xmx1024m", "-XX:ActiveProcessorCount=2",
               f"-Dlogging.config={args.minimal / 'conf' / 'logback.xml'}", "-cp",
               os.pathsep.join([str(args.admin / "conf"), str(args.admin / "lib" / "*")]),
               "org.apache.shenyu.admin.ShenyuAdminBootstrap", f"--server.port={port}", "--server.address=127.0.0.1",
               "--spring.profiles.active=h2", "--spring.datasource.url=jdbc:h2:mem:minimal_http_smoke;DB_CLOSE_DELAY=-1;MODE=MySQL",
               "--shenyu.sync.websocket.allowOrigins=ws://127.0.0.1", "--spring.jmx.enabled=false",
               "--shenyu.sync.websocket.token=" + sync_token,
               f"--logging.config={args.minimal / 'conf' / 'logback.xml'}",
               "--shenyu.jwt.secretKey=" + secrets.token_hex(32)]
    admin_process = subprocess.Popen(command, cwd=args.admin, stdout=log, stderr=subprocess.STDOUT)
    gateway, gateway_log, upstream = None, None, None
    admin = Admin(f"http://127.0.0.1:{port}")
    try:
        deadline = time.monotonic() + 120
        while True:
            if admin_process.poll() is not None:
                raise RuntimeError("Admin exited; inspect admin.log")
            try:
                admin.login()
                break
            except Exception:
                if time.monotonic() > deadline:
                    raise TimeoutError("Admin login did not become ready; inspect admin.log")
                time.sleep(0.25)
        upstream = Upstream("upstream-a")
        divide_id = next(plugin["id"] for plugin in admin.call("GET", f"/namespace-plugin/all/{NAMESPACE}") if plugin["name"] == "divide")
        registration = {"appName": "minimal-http-smoke", "contextPath": "/bench", "rpcType": "http",
                        "namespaceId": NAMESPACE, "host": "127.0.0.1", "port": upstream.port}
        admin.call("POST", "/shenyu-client/register-metadata", dict(registration, path="/bench/**", ruleName="minimal-http-smoke",
                   enabled=True, registerMetaData=False, pluginNames=[]))
        deadline = time.monotonic() + 30
        selectors = []
        while not selectors and time.monotonic() < deadline:
            selectors = admin.rows("selector", name="/bench", pluginId=divide_id)
            time.sleep(0.1)
        if len(selectors) != 1:
            raise RuntimeError(f"Expected one registered selector, found {len(selectors)}")
        selector_id = selectors[0]["id"]
        admin.call("POST", "/shenyu-client/register-uri", dict(registration, protocol="http://", eventType="REGISTER"))
        # The normal backend registration APIs create the selector, rule and local discovery.
        endpoint = SimpleNamespace(port=port, allow_origin="ws://127.0.0.1", sync_token=sync_token,
                                   snapshot_allowed=SimpleNamespace(is_set=lambda: True))
        launch_args = SimpleNamespace(java=args.java, cpu_affinity=None)
        gateway, gateway_log, base, ready = launch(launch_args, args.minimal, "minimal", endpoint,
                                                  args.minimal / "conf" / "application.yml", args.output / "gateway")
        checks = ["real Admin initial snapshot and HTTP success"]
        wait_for(base + "/bench/fail", lambda status, _: status == 503, gateway, 15)
        checks.append("upstream 503 preserved")
        rules = admin.rows("rule", selectorId=selector_id)
        if len(rules) != 1:
            raise RuntimeError(f"Expected one registered rule, found {len(rules)}")
        rule_id = rules[0]["id"]
        rule = admin.call("GET", f"/rule/{rule_id}")
        rule["namespaceId"] = NAMESPACE
        rule["ruleConditions"] = [condition("/bench/changed/**")]
        admin.call("PUT", f"/rule/{rule_id}", rule)
        wait_for(base + "/bench/ok", lambda _, body: error_code(body, -102), gateway, 15)
        wait_for(base + "/bench/changed/ok", lambda status, body: status == 200 and body == upstream.body, gateway, 15)
        checks.append("Admin rule update removes old path and enables new path")
        admin.call("DELETE", "/rule/batch", {"ids": [rule_id], "namespaceId": NAMESPACE})
        wait_for(base + "/bench/changed/ok", lambda _, body: error_code(body, -102), gateway, 15)
        checks.append("Admin rule deletion removes route")
        rule.pop("id", None)
        rule["ruleConditions"] = [condition("/bench/**")]
        admin.call("POST", "/rule", rule)
        wait_for(base + "/bench/ok", lambda status, body: status == 200 and body == upstream.body, gateway, 15)
        checks.append("Admin rule creation restores route")
        admin.call("DELETE", "/selector/batch", {"ids": [selector_id], "namespaceId": NAMESPACE})
        wait_for(base + "/bench/ok", lambda _, body: error_code(body, -107), gateway, 15)
        checks.append("Admin selector deletion removes route")
        (args.output / "report.json").write_text(json.dumps({"checks": checks, "readiness_s": ready,
            "admin_storage": "disposable in-memory H2", "synchronization": "real Admin WebSocket listener"}, indent=2) + "\n", encoding="utf-8")
        print(f"Real Admin/gateway smoke: {len(checks)} checks passed", flush=True)
    finally:
        if gateway:
            stop(gateway, gateway_log)
        stop(admin_process, log)
        if upstream:
            upstream.close()


if __name__ == "__main__":
    main()
