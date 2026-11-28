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

package org.apache.shenyu.plugin.agent.gateway.delegation;

import reactor.core.publisher.Mono;

/** Issues a request-local bearer credential for an explicitly configured HTTP tool. */
@FunctionalInterface
public interface AgentHttpIdentityDelegator {

    /**
     * Issue a credential for the trusted identity and fixed target in this snapshot.
     * @param request approved delegation request
     * @return one credential valid for this invocation
     */
    Mono<DelegatedCredential> delegate(DelegationRequest request);
}
