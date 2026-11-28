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

import java.security.PrivateKey;
import java.time.Instant;
import java.util.Objects;

/** Independently resolves and checks the signing key against the complete requested target. */
@FunctionalInterface
public interface DelegationSigningKeyResolver {

    /**
     * Resolve a fresh binding. The JWT delegator calls this on a worker that permits blocking.
     * @param request fixed target and requested policy
     * @return key and issuer bound to the target
     */
    SigningKey resolve(DelegationRequest request);

    /** Key material confined to the signer. */
    record SigningKey(String issuer, PrivateKey privateKey, Instant expiresAt) {

        public SigningKey(final String issuer, final PrivateKey privateKey) {
            this(issuer, privateKey, Instant.MAX);
        }

        public SigningKey {
            if (Objects.isNull(issuer) || issuer.isBlank() || Objects.isNull(privateKey) || Objects.isNull(expiresAt)) {
                throw new AgentIdentityDelegationException(AgentIdentityDelegationException.Reason.KEY_UNAVAILABLE);
            }
        }

        @Override
        public String toString() {
            return "SigningKey[REDACTED]";
        }
    }
}
