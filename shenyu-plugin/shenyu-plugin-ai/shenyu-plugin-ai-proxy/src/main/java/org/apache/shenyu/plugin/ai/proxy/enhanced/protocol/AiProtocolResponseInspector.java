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

import org.apache.shenyu.common.metrics.AiRequestObservation;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;

/**
 * Protocol-owned inspection of a raw provider response for usage and completion evidence.
 */
@FunctionalInterface
public interface AiProtocolResponseInspector {

    /**
     * Attach protocol-specific inspection without subscribing to or changing response bytes.
     *
     * @param response raw provider response
     * @param observation current upstream attempt observation
     * @param stream effective streaming mode
     * @return response with passive inspection attached
     */
    AiUpstreamResponse inspect(AiUpstreamResponse response, AiRequestObservation.Attempt observation, boolean stream);

    /**
     * Return an inspector that leaves the response untouched.
     *
     * @return identity inspector
     */
    static AiProtocolResponseInspector identity() {
        return (response, observation, stream) -> response;
    }
}
