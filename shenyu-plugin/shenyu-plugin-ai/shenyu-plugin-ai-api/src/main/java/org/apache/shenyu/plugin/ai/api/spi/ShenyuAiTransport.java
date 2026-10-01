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

package org.apache.shenyu.plugin.ai.api.spi;

import org.apache.shenyu.plugin.ai.api.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.api.model.AiUpstreamResponse;
import org.apache.shenyu.spi.SPI;

import reactor.core.publisher.Mono;

/** Shenyu AI upstream transport extension contract. */
@SPI
public interface ShenyuAiTransport {

    /**
     * Get the transport identifier used to discover this extension.
     *
     * @return transport identifier
     */
    String getName();

    /**
     * Execute an upstream request. Implementations should propagate cancellation and backpressure to the response body.
     *
     * @param request provider-prepared request
     * @return raw upstream response
     */
    Mono<AiUpstreamResponse> execute(AiUpstreamRequest request);
}
