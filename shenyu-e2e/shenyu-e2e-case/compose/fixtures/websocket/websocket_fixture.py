# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

from email import policy
from email.parser import BytesParser
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from threading import Thread
from urllib.parse import parse_qs, urlsplit

from websockets.sync.server import serve


def handshake(connection, request):
    path = urlsplit(request.path)
    if path.path == "/actuator/health":
        return connection.respond(HTTPStatus.OK, "UP\n")
    if path.path not in ("/myWebSocket", "/ws-native/myWebSocket"):
        return connection.respond(HTTPStatus.NOT_FOUND, "Unknown route\n")
    if len(parse_qs(path.query).get("token", [])) != 1:
        return connection.respond(HTTPStatus.FORBIDDEN, "One token is required\n")
    return None


def echo(connection):
    token = parse_qs(urlsplit(connection.request.path).query)["token"][0]
    for message in connection:
        connection.send(f"apache shenyu server send to {token} message : -> {message}")


class UploadHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path not in ("/ws/upload", "/ws-native/ws/upload"):
            self.send_error(HTTPStatus.NOT_FOUND)
            return
        if self.headers.get_content_type() != "multipart/form-data":
            self.send_error(HTTPStatus.UNSUPPORTED_MEDIA_TYPE)
            return
        length = int(self.headers.get("Content-Length", "0"))
        if not 0 < length <= 1024 * 1024:
            self.send_error(HTTPStatus.BAD_REQUEST, "Multipart content length is invalid")
            return
        envelope = (f"Content-Type: {self.headers['Content-Type']}\r\n\r\n").encode("ascii")
        form = BytesParser(policy=policy.default).parsebytes(envelope + self.rfile.read(length))
        if not any(part.get_param("name", header="content-disposition") == "file"
                   and part.get_filename() and part.get_payload(decode=True) for part in form.iter_parts()):
            self.send_error(HTTPStatus.BAD_REQUEST, "A file is required")
            return
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", "2")
        self.end_headers()
        self.wfile.write(b"ok")


if __name__ == "__main__":
    upload = ThreadingHTTPServer(("0.0.0.0", 8870), UploadHandler)
    Thread(target=upload.serve_forever, daemon=True).start()
    with serve(echo, "0.0.0.0", 8868, process_request=handshake) as server:
        server.serve_forever()
