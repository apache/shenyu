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

package org.apache.shenyu.plugin.ai.proxy.enhanced.protocol;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

/**
 * Contract for decoding a client AI protocol into ShenYu-owned models.
 */
public interface AiProxyProtocol {

    /**
     * Protocol identifier.
     *
     * @return protocol identifier
     */
    String name();

    /**
     * Determine whether the JSON body uses this protocol.
     *
     * @param payload request JSON
     * @return whether it matches
     */
    boolean matches(JsonNode payload);

    /**
     * Decode client request into ShenYu internal request.
     *
     * @param rawBody original request body
     * @param payload parsed request JSON
     * @return internal AI request
     */
    ShenyuAiRequest decodeRequest(
            String rawBody,
            ObjectNode payload);
}
