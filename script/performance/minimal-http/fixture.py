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

"""Loopback upstreams and a deterministic Admin WebSocket protocol fixture.

This exercises the packaged gateway's real synchronization and proxy paths.
It does not claim coverage of Admin's database, REST API, or authentication.
"""

import json
import socket
import threading
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

from websockets.exceptions import ConnectionClosed
from websockets.sync.server import serve

NAMESPACE = "649330b6-c2d7-4edc-be8e-8a54df9eb385"


def condition(path):
    return {"paramType": "uri", "operator": "match", "paramName": "/", "paramValue": path}


def configuration(port, selector_id="minimal-http-selector"):
    base = {"namespaceId": NAMESPACE, "enabled": True, "sort": 1}
    selector = dict(base, id=selector_id, pluginId="divide", pluginName="divide",
                    name="minimal-http", type=1, matchMode=0, logged=False, continued=True,
                    handle="[]", conditionList=[condition("/bench/**")])
    rule = dict(base, id="minimal-http-rule", selectorId=selector["id"], pluginName="divide",
                name="minimal-http", matchMode=0, loged=False,
                handle=json.dumps({"loadBalance": "roundRobin", "retry": 0, "timeout": 1000}),
                conditionDataList=[condition("/bench/**")])
    discovery = {"namespaceId": NAMESPACE, "pluginName": "divide", "selectorId": selector["id"],
                 "selectorName": selector["name"], "upstreamDataList": [
                     {"id": "minimal-http-upstream", "protocol": "http://", "url": f"127.0.0.1:{port}",
                      "status": 0, "weight": 100, "props": json.dumps({"healthCheckEnabled": "false"}), "namespaceId": NAMESPACE}]}
    return {"PLUGIN": [dict(base, id="divide", name="divide", config="{}", role="Proxy", sort=200)],
            "SELECTOR": [selector], "RULE": [rule], "DISCOVER_UPSTREAM": [discovery]}


class Upstream:
    def __init__(self, name, response_bytes=256):
        self.body = (name + ":" + "x" * response_bytes)[:response_bytes].encode()
        self.hits = 0
        fixture = self

        class Handler(BaseHTTPRequestHandler):
            protocol_version = "HTTP/1.1"

            def handle(self):
                try:
                    super().handle()
                except (BrokenPipeError, ConnectionResetError, ConnectionAbortedError):
                    # k6 closes persistent connections when a scenario ends.
                    pass

            def setup(self):
                super().setup()
                self.connection.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

            def do_GET(self):
                fixture.hits += 1
                status = 503 if self.path.startswith("/bench/fail") else 200
                self.send_response(status)
                self.send_header("Content-Type", "text/plain")
                self.send_header("Content-Length", str(len(fixture.body)))
                self.end_headers()
                self.wfile.write(fixture.body)

            def log_message(self, *_):
                pass

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.port = self.server.server_port

    def close(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=5)


class SyncFixture:
    def __init__(self, upstream_port, hold_snapshot=False):
        self.lock = threading.RLock()
        self.snapshot_allowed = threading.Event()
        if not hold_snapshot:
            self.snapshot_allowed.set()
        self.connections = set()
        self.selector_id = "minimal-http-selector"
        self.data = configuration(upstream_port)
        self.server = serve(self.handle, "127.0.0.1", 0)
        self.port = self.server.socket.getsockname()[1]
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    @staticmethod
    def message(group, event, data):
        return json.dumps({"groupType": group, "eventType": event, "data": data,
                           "namespaceId": NAMESPACE,
                           "fullSnapshot": event == "MYSELF" and group in {"PLUGIN", "SELECTOR", "RULE"}})

    def handle(self, connection):
        try:
            for request in connection:
                if request == "MYSELF" or request.startswith("MYSELF_V1:"):
                    if not self.snapshot_allowed.wait(timeout=60):
                        return
                    with self.lock:
                        framed = request.startswith("MYSELF_V1:")
                        request_id = request.partition(":")[2]
                        for sequence, (group, rows) in enumerate(self.data.items()):
                            payload = self.message(group, "MYSELF", rows)
                            connection.send(json.dumps({"eventType": "INITIAL_SYNC_V1", "requestId": request_id,
                                                        "sequence": sequence, "payload": payload}) if framed else payload)
                        if framed:
                            connection.send(json.dumps({"eventType": "INITIAL_SYNC_V1", "requestId": request_id,
                                                        "sequence": len(self.data)}))
                        self.connections.add(connection)
        except ConnectionClosed:
            pass
        finally:
            with self.lock:
                self.connections.discard(connection)

    def update(self, group, rows, event="UPDATE"):
        with self.lock:
            self.data[group] = [] if event == "DELETE" else rows
            for connection in list(self.connections):
                try:
                    connection.send(self.message(group, event, rows))
                except ConnectionClosed:
                    self.connections.discard(connection)

    def reset(self, upstream_port):
        if not self.data["SELECTOR"]:
            # Admin allocates a new selector ID when a deleted route is recreated.
            self.selector_id = "minimal-http-" + uuid.uuid4().hex
        for group, rows in configuration(upstream_port, self.selector_id).items():
            self.update(group, rows)

    def upstreams(self, port):
        return configuration(port, self.selector_id)["DISCOVER_UPSTREAM"]

    def close(self):
        self.snapshot_allowed.set()
        with self.lock:
            for connection in list(self.connections):
                connection.close()
        self.server.shutdown()
        self.thread.join(timeout=5)
