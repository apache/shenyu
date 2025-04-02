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

package org.apache.shenyu.plugin.httpclient;


import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 * Retry Policy Interface.
 * @param <R> Request Response Type
 *@Date 2025/3/23 08:27
 */
public interface RetryStrategy<R> {
    /**
     * Execute retry policy.
     *
     * @param clientResponse Original Request Response
     * @param exchange       Server Exchange Object
     * @param duration       Timeout
     * @param retryTimes     Number of retries
     * @return Number of retries
     */
    Mono<R> execute(Mono<R> clientResponse, ServerWebExchange exchange, Duration duration, int retryTimes);
}
