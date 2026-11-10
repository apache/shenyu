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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiResponseAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;

/**
 * Prepared request-scoped AI exchange passed from request planning to execution.
 *
 * @param request provider-ready upstream request
 * @param responseAdapter request-scoped response adaptation chain
 * @param clientProtocol client-facing protocol
 * @param targetProtocol provider-facing protocol
 * @param stream effective streaming mode
 * @param conservativeReplaySafe whether the prepared request has no known tool-bearing fields
 * @param operation prepared AI operation
 * @param providerDeclaredReplaySafe whether the Provider declares the operation replay-safe
 * @param provider selected Provider identifier
 * @param cacheCompatible whether the prepared exchange is compatible with exact response caching
 */
record AiPreparedExchange(
        AiUpstreamRequest request,
        AiResponseAdapter responseAdapter,
        String clientProtocol,
        String targetProtocol,
        boolean stream,
        boolean conservativeReplaySafe,
        ShenyuAiRequest.Operation operation,
        boolean providerDeclaredReplaySafe,
        String provider,
        boolean cacheCompatible) {
}
