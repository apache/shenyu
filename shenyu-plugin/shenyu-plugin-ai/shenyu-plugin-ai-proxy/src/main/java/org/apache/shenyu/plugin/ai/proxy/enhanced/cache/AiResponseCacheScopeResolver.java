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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Application-owned, nonblocking cache authorization adapter. Empty means bypass.
 * Implementations must allow only approved stateless text routes and derive identity,
 * data-access scope and policy/prompt revisions from trusted server-side state.
 * Authentication and authorization still run on every request, including cache hits.
 * Failures propagate rather than falling back to unauthenticated execution.
 */
@FunctionalInterface
public interface AiResponseCacheScopeResolver {

    /**
     * Authorize response reuse for the current request and matched route.
     * Never use arbitrary tenant headers or a diagnostic snapshot as authorization.
     *
     * @param exchange current exchange
     * @param selectorId matched AI selector
     * @param ruleId matched AI rule
     * @return an approved scope, empty to bypass, or an authorization error
     */
    Mono<AiResponseCacheScope> resolve(ServerWebExchange exchange, String selectorId, String ruleId);
}
