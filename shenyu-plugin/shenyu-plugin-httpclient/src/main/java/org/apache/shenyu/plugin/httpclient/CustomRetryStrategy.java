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

import java.time.Duration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 *  Custom retry policy.
 *      Also please achieve your own
 */
public class CustomRetryStrategy<R> implements RetryStrategy<R> {
    private static final Logger LOG = LoggerFactory.getLogger(CustomRetryStrategy.class);

    private final AbstractHttpClientPlugin<R> httpClientPlugin;

    public CustomRetryStrategy(final AbstractHttpClientPlugin<R> httpClientPlugin) {
        this.httpClientPlugin = httpClientPlugin;
    }

    /**
     * Custom retry policy.
     *
     * @param response   he Mono object of the response
     * @param exchange   Current Server Exchange Object
     * @param duration   Timeout
     * @param retryTimes Number of retries
     * @return Response Mono object after retry processing
     */
    public Mono<R> execute(final Mono<R> response, final ServerWebExchange exchange, final Duration duration, final int retryTimes) {
        return null;
    }
}
