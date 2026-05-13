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

import org.slf4j.Logger;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.Objects;

/**
 * Shared utility for logging upstream AI service errors with WebClientResponseException details.
 */
public final class UpstreamErrorLogger {

    private UpstreamErrorLogger() {
    }

    public static void logUpstreamError(final Logger log, final Throwable e, final String mode) {
        final WebClientResponseException webClientEx = findWebClientResponseException(e);
        if (Objects.nonNull(webClientEx)) {
            log.error("[AiProxy] {} failed, status={}, upstreamBody={}",
                    mode, webClientEx.getStatusCode(), webClientEx.getResponseBodyAsString(), e);
        } else {
            log.error("[AiProxy] {} failed", mode, e);
        }
    }

    private static WebClientResponseException findWebClientResponseException(final Throwable e) {
        Throwable current = e;
        while (Objects.nonNull(current)) {
            if (current instanceof WebClientResponseException ex) {
                return ex;
            }
            current = current.getCause();
        }
        return null;
    }
}
