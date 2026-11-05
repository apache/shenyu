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

package org.apache.shenyu.plugin.agent.gateway.governance;

import reactor.core.publisher.Mono;

import java.util.Objects;
import java.time.Instant;

/** Backend port for distributed atomic MCP quota admission. */
public interface AgentMcpQuotaStore {

    /** Atomically consume one invocation and acquire one bounded concurrency lease.
     *
     * @param policy trusted policy snapshot
     * @param tenant trusted tenant
     * @param subject trusted subject
     * @param correlationId unique logical request identity
     * @param now admission time
     * @return atomic admission result
     */
    Mono<Admission> admit(AgentMcpQuotaConfig policy, String tenant, String subject, String correlationId, Instant now);

    /** Idempotently release one lease owned by this request.
     *
     * @param policy trusted policy snapshot
     * @param tenant trusted tenant
     * @param subject trusted subject
     * @param correlationId unique logical request identity
     * @param lease unique opaque lease returned by admission
     * @return true when this call released the lease
     */
    Mono<Boolean> release(AgentMcpQuotaConfig policy, String tenant, String subject, String correlationId, String lease);

    /** Result of an atomic invocation and concurrency admission.
     *
     * @param allowed whether both dimensions admitted the request
     * @param invocationLimited invocation count would exceed its policy
     * @param concurrencyLimited concurrency would exceed its policy
     * @param lease opaque lease required for release
     */
    record Admission(boolean allowed, boolean invocationLimited, boolean concurrencyLimited, String lease) {
        public Admission {
            if (allowed && (invocationLimited || concurrencyLimited)) {
                throw new IllegalArgumentException("An admitted decision cannot carry a limit flag");
            }
            if (!allowed && invocationLimited == concurrencyLimited) {
                throw new IllegalArgumentException("A rejected decision must identify one first limit");
            }
            if (allowed && Objects.isNull(lease)) {
                throw new IllegalArgumentException("An admitted decision requires a lease");
            }
            if (allowed && (lease.isBlank() || lease.length() > 128 || !lease.equals(lease.strip())
                    || lease.chars().anyMatch(Character::isISOControl))) {
                throw new IllegalArgumentException("An admitted lease must be bounded nonblank text");
            }
            if (!allowed && !Objects.toString(lease, "").isEmpty()) {
                throw new IllegalArgumentException("A rejected decision must not retain a lease");
            }
        }
    }
}
