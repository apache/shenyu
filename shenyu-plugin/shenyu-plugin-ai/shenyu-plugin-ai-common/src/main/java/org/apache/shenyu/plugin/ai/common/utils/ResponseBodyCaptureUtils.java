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

package org.apache.shenyu.plugin.ai.common.utils;

import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Response body capture utils.
 */
public class ResponseBodyCaptureUtils {

    /**
     * Capture response body from exchange.
     * Note: This method returns an empty string because the response body
     * is not available at the time of template assembly.
     * The actual response body will be captured in the response decorator.
     *
     * @param exchange the exchange
     * @return the captured response body (empty string)
     */
    public static Mono<String> captureResponseBody(final ServerWebExchange exchange) {
        return Mono.just("");
    }
} 
