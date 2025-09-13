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

package org.apache.shenyu.plugin.ai.common.strategy;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Fallback strategy.
 */
public interface FallbackStrategy {

    /**
     * Execute the fallback strategy.
     *
     * @param fallbackClient the pre-configured and cached chat client for fallback
     * @param requestBody    the original request body as a string
     * @param originalError  the original error that triggered the fallback
     * @return a Mono containing the fallback ChatResponse
     */
    Mono<ChatResponse> fallback(ChatClient fallbackClient, String requestBody, Throwable originalError);

    /**
     * Execute the fallback strategy for stream.
     *
     * @param fallbackClient the pre-configured and cached chat client for fallback
     * @param requestBody    the original request body as a string
     * @param originalError  the original error that triggered the fallback
     * @return a Flux containing the fallback ChatResponse
     */
    Flux<ChatResponse> fallbackStream(ChatClient fallbackClient, String requestBody, Throwable originalError);
}