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

package org.apache.shenyu.plugin.ai.protocol;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.List;
import java.util.Map;

/**
 * The detection evidence of one AI protocol: the request paths that identify it,
 * optional exact header hints and the request body predicate of last resort.
 */
public interface AiProtocolDescriptor {

    /**
     * The protocol identifier, one of {@link AiProtocols}.
     *
     * @return the protocol identifier
     */
    String id();

    /**
     * The normalized request paths that identify the protocol.
     *
     * @return the paths, matched exactly or as a suffix of the request path
     */
    List<String> paths();

    /**
     * The exact header hints that identify the protocol, header name to exact value.
     *
     * @return the hints, an empty map means the protocol offers no header evidence
     */
    Map<String, String> headerHints();

    /**
     * Whether the request body matches the protocol shape.
     *
     * @param payload the parsed request body
     * @return true if the payload carries the protocol shape
     */
    boolean bodyMatches(JsonNode payload);
}
