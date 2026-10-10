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

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiResponse;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiStreamEvent;
import org.apache.shenyu.spi.SPI;

import reactor.core.publisher.Flux;

/** Shenyu AI protocol extension contract. */
@SPI
public interface ShenyuAiProtocol {

    /**
     * Get the protocol identifier used to discover this extension.
     *
     * @return protocol identifier
     */
    String getName();

    /**
     * Decode a client request while retaining its original payload.
     *
     * @param payload client request payload
     * @return normalized request
     */
    ShenyuAiRequest decodeRequest(JsonNode payload);

    /**
     * Encode a normalized response in this client protocol.
     *
     * @param response normalized response
     * @return client response payload
     */
    JsonNode encodeResponse(ShenyuAiResponse response);

    /**
     * Encode normalized streaming events as client protocol events.
     *
     * @param events normalized stream events
     * @return protocol stream events
     */
    Flux<JsonNode> encodeStream(Flux<ShenyuAiStreamEvent> events);
}
