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

import java.util.Set;

import org.apache.shenyu.plugin.ai.api.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.api.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiResponse;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiStreamEvent;
import org.apache.shenyu.spi.SPI;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/** Shenyu AI provider extension contract. */
@SPI
public interface ShenyuAiProvider {

    /**
     * Get the provider identifier used to discover this extension.
     *
     * @return provider identifier
     */
    String getName();

    /**
     * Get the protocol identifiers supported by this provider.
     *
     * @return supported protocol identifiers
     */
    Set<String> getSupportedProtocols();

    /**
     * Prepare the upstream endpoint, headers, and body for a normalized request.
     *
     * @param request normalized request
     * @return upstream request
     */
    AiUpstreamRequest createRequest(ShenyuAiRequest request);

    /**
     * Convert a raw upstream response to the normalized response model.
     *
     * @param response raw upstream response
     * @return normalized response
     */
    Mono<ShenyuAiResponse> decodeResponse(AiUpstreamResponse response);

    /**
     * Convert a raw upstream stream to normalized events without buffering the entire body.
     *
     * @param response raw upstream response
     * @return normalized response events
     */
    Flux<ShenyuAiStreamEvent> decodeStream(AiUpstreamResponse response);
}
