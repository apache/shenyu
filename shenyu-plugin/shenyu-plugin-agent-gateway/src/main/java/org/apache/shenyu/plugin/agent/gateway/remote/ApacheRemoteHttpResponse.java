/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.agent.gateway.remote;

import org.apache.http.Header;
import org.apache.http.HttpResponse;

import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/** Immutable HTTP response adapter used by the target-owned Apache transport. */
final class ApacheRemoteHttpResponse {

    private final int status;

    private final Map<String, List<String>> headers;

    private final ByteArrayOutputStream body;

    private ApacheRemoteHttpResponse(final int status, final Map<String, List<String>> headers, final ByteArrayOutputStream body) {
        this.status = status;
        this.headers = Map.copyOf(headers);
        this.body = body;
    }

    static ApacheRemoteHttpResponse from(final HttpResponse response, final ByteArrayOutputStream body) {
        Map<String, List<String>> values = new java.util.TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        for (Header header : response.getAllHeaders()) {
            values.computeIfAbsent(header.getName().toLowerCase(Locale.ROOT), ignored -> new java.util.ArrayList<>()).add(header.getValue());
        }
        return new ApacheRemoteHttpResponse(response.getStatusLine().getStatusCode(), values, body);
    }

    int status() {
        return status;
    }

    Map<String, List<String>> headers() {
        return headers;
    }

    byte[] body() {
        return body.toByteArray();
    }

}
