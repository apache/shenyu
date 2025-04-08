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

import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

/**
 * The interface Ai model.
 */
public interface AiModel {
    
    /**
     * Invoke ai.
     *
     * @param aiCommonConfig  the ai config
     * @param exchange       the exchange
     * @param chain          the chain
     * @param messageReaders the message readers
     * @return the mono
     */
    Mono<Void> invoke(AiCommonConfig aiCommonConfig,
                      ServerWebExchange exchange,
                      ShenyuPluginChain chain,
                      List<HttpMessageReader<?>> messageReaders);
    
    /**
     * getCompletionTokens.
     *
     * @param responseBody the response body
     * @return  the completion tokens
     */
    Long getCompletionTokens(String responseBody);
}
