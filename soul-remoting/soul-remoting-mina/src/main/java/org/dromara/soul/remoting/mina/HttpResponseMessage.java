/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.remoting.mina;

import org.apache.mina.core.buffer.IoBuffer;
import org.apache.mina.http.api.DefaultHttpResponse;
import org.apache.mina.http.api.HttpStatus;
import org.apache.mina.http.api.HttpVersion;

import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;

/**
 * HttpResponseMessage .
 *
 * @author sixh
 */
public class HttpResponseMessage extends DefaultHttpResponse {

    private String body;
    private static final CharsetEncoder ENCODER = StandardCharsets.UTF_8.newEncoder();

    public void setBody(String body) {
        this.body = body;
    }

    /**
     * Creates a new DefaultHttpResponse instance
     *
     * @param version The HTTP version
     * @param status  The HTTP status
     * @param headers The HTTP headers
     */
    public HttpResponseMessage(HttpVersion version, HttpStatus status, Map<String, String> headers) {
        super(version, status, headers);
    }

    IoBuffer buffer() throws CharacterCodingException {
        StringBuilder sb = new StringBuilder(this.getStatus().line());
        for (Map.Entry<String, String> header : this.getHeaders().entrySet()) {
            sb.append(header.getKey());
            sb.append(": ");
            sb.append(header.getValue());
            sb.append("\r\n");
        }
        sb.append("\r\n");
        sb.append(body);
        IoBuffer buf = IoBuffer.allocate(sb.length()).setAutoExpand(true);
        buf.putString(sb.toString(), ENCODER);
        buf.flip();
        return buf;
    }
}
