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

package org.apache.shenyu.plugin.ai.proxy.enhanced.model;

import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Map;

/**
 * Protocol-independent response received from an AI upstream.
 */
public final class AiUpstreamResponse {

    private final int statusCode;

    private final Map<String, List<String>> headers;

    private final Flux<byte[]> body;

    /**
     * Create an upstream response.
     *
     * @param statusCode HTTP status code
     * @param headers response headers
     * @param body reactive response body
     */
    public AiUpstreamResponse(final int statusCode, final Map<String, List<String>> headers,
            final Flux<byte[]> body) {
        this.statusCode = statusCode;
        this.headers = headers;
        this.body = body;
    }

    /**
     * Get HTTP status code.
     *
     * @return HTTP status code
     */
    public int getStatusCode() {
        return statusCode;
    }

    /**
     * Get response headers.
     *
     * @return response headers
     */
    public Map<String, List<String>> getHeaders() {
        return headers;
    }

    /**
     * Get reactive response body.
     *
     * @return reactive response body
     */
    public Flux<byte[]> getBody() {
        return body;
    }
}
