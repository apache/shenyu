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

package org.apache.shenyu.plugin.agent.gateway.remote;

import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

/** Immutable target and independently resolved credential binding. */
public final class RemoteServerBinding {

    private static final Logger LOG = LoggerFactory.getLogger(RemoteServerBinding.class);

    private final Config config;

    private final String authorization;

    private RemoteServerBinding(final Config config, final Credential credential) {
        this.config = config;
        authorization = credential.authorization;
    }

    /**
     * resolve.
     * @param config trusted config value
     * @param allowedEndpoints trusted allowedEndpoints value
     * @param localResolver trusted localResolver value
     * @return operation result
     */
    public static RemoteServerBinding resolve(final Config config, final Set<URI> allowedEndpoints, final Function<Config, Credential> localResolver) {
        if (Objects.isNull(config) || Objects.isNull(allowedEndpoints) || Objects.isNull(localResolver)) {
            throw new IllegalArgumentException("Missing controlled target configuration");
        }
        // Exact configured URI, including path/port. Never resolve an Agent-provided URL.
        if (!Set.copyOf(allowedEndpoints).contains(config.endpoint())) {
            throw new SecurityException("Target is not in the configured allowlist");
        }
        Credential credential;
        try {
            credential = localResolver.apply(config);
        } catch (RuntimeException error) {
            // A secret-store exception can contain credentials: do not retain its message/cause.
            throw new SecurityException("Service credential resolution failed");
        }
        // Credential binding identity is the target reference only: budgets are trusted transport
        // configuration, so a resolver which never copies them keeps resolving every configured value.
        if (Objects.isNull(credential) || !config.bindsTo(credential.target)) {
            throw new SecurityException("Missing or mismatched service credential binding");
        }
        return new RemoteServerBinding(config, credential);
    }

    /**
     * config.
     * @return operation result
     */
    public Config config() {
        return config;
    }

    String authorization() {
        return authorization;
    }

    /**
     * new Client.
     * @return operation result
     */
    public RequestScopedMcpClient newClient() {
        return new RequestScopedMcpClient(this, event -> {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Remote MCP response server={} method={} id={} status={} bytes={}",
                        event.get("server"), event.get("method"), event.get("id"), event.get("status"), event.get("bytes"));
            }
        });
    }

    /**
     * lifecycle Config.
     * @return operation result
     */
    public RemoteCatalogLifecycle.Config lifecycleConfig() {
        return new RemoteCatalogLifecycle.Config(config.name(), config.credentialVersion(), this::newClient);
    }

    @Override
    public String toString() {
        return "RemoteServerBinding[" + config.name() + ", " + config.credentialVersion() + ", credential=REDACTED]";
    }

    /**
     * Immutable target and credential binding, including the trusted payload budget snapshot.
     * <p>Record equality, and therefore {@code hashCode}, covers the complete snapshot including
     * both budgets: two targets configured with different budgets are different values. Credential
     * matching deliberately does not use that equality, because budgets are not part of the secret
     * reference; {@link #bindsTo(Config)} defines the binding identity and ignores the budgets.</p>
     */
    public record Config(String name, URI endpoint, String credentialRef, String credentialVersion,
                         int requestBytes, int responseBytes) {

        public Config {
            if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid stable server name");
            }
            RemoteTransportPolicy.endpoint(endpoint);
            if (
                Objects.isNull(credentialRef)
                    || !credentialRef.matches("[A-Za-z0-9_./-]{1,96}")
                    || Objects.isNull(credentialVersion)
                    || !credentialVersion.matches("[A-Za-z0-9_-]{1,48}")
            ) {
                throw new IllegalArgumentException("Invalid credential reference or version");
            }
            AgentGatewayAggregationConfig.validateRemotePayloadBudget(requestBytes, responseBytes);
        }

        /**
         * Legacy binding without explicit budgets: keep the fixed 8 KiB transport limit.
         * @param name stable server name
         * @param endpoint exact configured endpoint
         * @param credentialRef service credential reference
         * @param credentialVersion service credential version
         */
        public Config(final String name, final URI endpoint, final String credentialRef, final String credentialVersion) {
            this(name, endpoint, credentialRef, credentialVersion,
                    AgentGatewayAggregationConfig.DEFAULT_REMOTE_REQUEST_BYTES,
                    AgentGatewayAggregationConfig.DEFAULT_REMOTE_RESPONSE_BYTES);
        }

        /**
         * Whether a resolved credential refers to this exact target.
         * Only the name, endpoint, credential reference and credential version identify a binding;
         * the payload budgets are trusted transport settings, so they never invalidate a credential.
         * @param other target reported by the service credential resolver
         * @return true when the credential binding identity matches
         */
        public boolean bindsTo(final Config other) {
            return Objects.nonNull(other)
                    && name.equals(other.name)
                    && endpoint.equals(other.endpoint)
                    && credentialRef.equals(other.credentialRef)
                    && credentialVersion.equals(other.credentialVersion);
        }
    }

    /** No generated record accessors/toString exposing a bearer token. */
    public static final class Credential {

        private final Config target;

        private final String authorization;

        public Credential(final Config target, final String bearerToken) {
            if (Objects.isNull(target) || Objects.isNull(bearerToken) || bearerToken.length() > 2048 || !bearerToken.matches("[A-Za-z0-9._~+/-]+=*")) {
                throw new IllegalArgumentException("Invalid service credential");
            }
            this.target = target;
            authorization = "Bearer " + bearerToken;
        }

        @Override
        public String toString() {
            return "ServiceCredential[REDACTED]";
        }
    }
}
