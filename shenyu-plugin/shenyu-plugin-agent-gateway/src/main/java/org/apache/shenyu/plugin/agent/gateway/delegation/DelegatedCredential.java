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

import java.time.Instant;
import java.util.Objects;
import java.util.regex.Pattern;

/** Single-invocation credential safe to carry in an HTTP Authorization header. */
public final class DelegatedCredential {

    private static final Pattern BEARER_TOKEN = Pattern.compile("[A-Za-z0-9._~+/-]+=*");

    private final String bearerToken;

    private final Instant expiresAt;

    public DelegatedCredential(final String bearerToken, final Instant expiresAt) {
        if (Objects.isNull(bearerToken) || !BEARER_TOKEN.matcher(bearerToken).matches() || Objects.isNull(expiresAt)) {
            throw new AgentIdentityDelegationException(AgentIdentityDelegationException.Reason.SIGNING_FAILED);
        }
        this.bearerToken = bearerToken;
        this.expiresAt = expiresAt;
    }

    public String getBearerToken() {
        return bearerToken;
    }

    public Instant getExpiresAt() {
        return expiresAt;
    }

    @Override
    public String toString() {
        return "DelegatedCredential[REDACTED]";
    }
}
