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

package org.apache.shenyu.plugin.ai.proxy.enhanced.conversion;

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

/**
 * Converts a client protocol request to a target protocol and binds reverse response conversion.
 */
public interface AiProtocolConverter {

    /**
     * Get the source client protocol identifier.
     *
     * @return source protocol identifier
     */
    String sourceProtocol();

    /**
     * Get the target upstream protocol identifier.
     *
     * @return target protocol identifier
     */
    String targetProtocol();

    /**
     * Create one request-scoped bidirectional conversion.
     *
     * @param request source protocol request
     * @param stream effective streaming mode
     * @return converted request and its reverse response adapter
     */
    AiProtocolConversion convert(ShenyuAiRequest request, boolean stream);
}
