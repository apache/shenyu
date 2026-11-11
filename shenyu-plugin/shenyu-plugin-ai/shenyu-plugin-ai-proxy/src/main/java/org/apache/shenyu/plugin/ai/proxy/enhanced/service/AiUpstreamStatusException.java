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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Signals an upstream HTTP status that must be handled by retry or fallback orchestration.
 */
final class AiUpstreamStatusException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final int status;

    private final Map<String, List<String>> headers;

    /**
     * Create an upstream status exception.
     *
     * @param status upstream HTTP status
     * @param headers upstream response headers
     */
    AiUpstreamStatusException(final int status, final Map<String, List<String>> headers) {
        super("AI upstream returned HTTP " + status);
        this.status = status;
        this.headers = Objects.requireNonNull(headers, "headers must not be null");
    }

    /**
     * Get the upstream HTTP status.
     *
     * @return upstream status
     */
    int status() {
        return status;
    }

    /**
     * Get the upstream response headers.
     *
     * @return upstream response headers
     */
    Map<String, List<String>> headers() {
        return headers;
    }
}
