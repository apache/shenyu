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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.Objects;

/**
 * Normalized client response for an AI proxy failure.
 */
final class AiProxyFailureResponse {

    private final HttpStatus status;

    private final HttpHeaders headers;

    AiProxyFailureResponse(final HttpStatus status) {
        this(status, HttpHeaders.EMPTY);
    }

    AiProxyFailureResponse(final HttpStatus status, final HttpHeaders headers) {
        this.status = Objects.requireNonNull(status, "status must not be null");
        this.headers = HttpHeaders.readOnlyHttpHeaders(Objects.requireNonNull(headers, "headers must not be null"));
    }

    HttpStatus getStatus() {
        return status;
    }

    HttpHeaders getHeaders() {
        return headers;
    }
}
