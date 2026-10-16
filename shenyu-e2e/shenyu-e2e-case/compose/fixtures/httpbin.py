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

"""Local upstream for the Divide URI and HTTP method routing scenarios."""

import json
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit


class HttpbinHandler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def respond(self):
        path = urlsplit(self.path)
        data = self.rfile.read(int(self.headers.get("Content-Length", "0")))
        status = 200
        if path.path == "/status/200":
            body = b""
        elif path.path == "/anything" or path.path.startswith("/anything/"):
            body = json.dumps({
                "method": self.command,
                "url": self.path,
                "args": parse_qs(path.query),
                "data": data.decode("utf-8"),
            }).encode("utf-8")
        else:
            status = 404
            body = b'{"error":"unknown fixture endpoint"}'
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)

    do_GET = respond
    do_POST = respond
    do_PUT = respond
    do_DELETE = respond
    do_HEAD = respond


if __name__ == "__main__":
    ThreadingHTTPServer(("0.0.0.0", 80), HttpbinHandler).serve_forever()
