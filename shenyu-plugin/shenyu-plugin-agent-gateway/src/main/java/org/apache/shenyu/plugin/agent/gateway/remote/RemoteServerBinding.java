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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Objects;
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
     * @param routes trusted approved routes
     * @param localResolver trusted localResolver value
     * @return operation result
     */
    public static RemoteServerBinding resolve(final Config config, final RemoteTransportRouteSet routes,
                                              final Function<Config, Credential> localResolver) {
        if (Objects.isNull(config) || Objects.isNull(routes) || Objects.isNull(localResolver)) {
            throw new IllegalArgumentException("Missing controlled target configuration");
        }
        // Exact configured URI, including path/port, must also have a deployment-approved address.
        routes.route(config.endpoint());
        Credential credential;
        try {
            credential = localResolver.apply(config);
        } catch (RuntimeException error) {
            // A secret-store exception can contain credentials: do not retain its message/cause.
            throw new SecurityException("Service credential resolution failed");
        }
        if (Objects.isNull(credential) || !config.equals(credential.target)) {
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
     * new Client bound to the historical fixed-IP route.
     * @return operation result
     */
    public RequestScopedMcpClient newClient() {
        return newClient(RemoteTransportRoute.fixed(config.endpoint()));
    }

    /**
     * new Client bound to an approved route.
     * @param route trusted immutable route
     * @return operation result
     */
    public RequestScopedMcpClient newClient(final RemoteTransportRoute route) {
        return newClient(route, null);
    }

    /**
     * new Client with an explicit test-only TLS context.
     * @param route trusted immutable route
     * @param testContext fixture TLS context
     * @return operation result
     */
    RequestScopedMcpClient newClient(final RemoteTransportRoute route, final javax.net.ssl.SSLContext testContext) {
        return new RequestScopedMcpClient(this, route, event -> {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Remote MCP response server={} method={} id={} status={} bytes={}",
                        event.get("server"), event.get("method"), event.get("id"), event.get("status"), event.get("bytes"));
            }
        }, testContext);
    }

    /**
     * lifecycle Config.
     * @return operation result
     */
    public RemoteCatalogLifecycle.Config lifecycleConfig() {
        return lifecycleConfig(RemoteTransportRoute.fixed(config.endpoint()));
    }

    /**
     * lifecycle Config with deployment-approved route.
     * @param route trusted immutable route
     * @return operation result
     */
    public RemoteCatalogLifecycle.Config lifecycleConfig(final RemoteTransportRoute route) {
        return lifecycleConfig(route, null);
    }

    /**
     * lifecycle Config with a verification-only TLS context.
     * @param route trusted immutable route
     * @param verificationTlsContext fixture TLS context, never production configuration
     * @return lifecycle configuration
     */
    RemoteCatalogLifecycle.Config lifecycleConfig(final RemoteTransportRoute route, final javax.net.ssl.SSLContext verificationTlsContext) {
        return new RemoteCatalogLifecycle.Config(config.name(), config.credentialVersion(), () -> newClient(route, verificationTlsContext));
    }

    @Override
    public String toString() {
        return "RemoteServerBinding[" + config.name() + ", " + config.credentialVersion() + ", credential=REDACTED]";
    }

    public record Config(String name, URI endpoint, String credentialRef, String credentialVersion) {
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
