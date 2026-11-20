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

package org.apache.shenyu.plugin.httpclient.config;

import reactor.netty.http.client.HttpClient;
import org.springframework.web.reactive.function.client.WebClient;

/**
 * Startup-time registry of the named upstream connection-pool groups configured through
 * {@code shenyu.httpclient.pool-groups}.
 *
 * <p>The registry is immutable and startup-only: groups are built exclusively from trusted
 * configuration, never from request ids or headers, and changing the set of groups requires
 * a restart. An empty registry (the default deployment) means every request takes the
 * shared default client and behaviour is unchanged. After {@link #close()} every lookup is
 * rejected, so callers never receive a client backed by a disposed provider.</p>
 */
public interface UpstreamPoolGroupRegistry extends AutoCloseable {

    /**
     * Whether the named group exists.
     *
     * @param name the group name from a divide rule handle
     * @return true when the group was configured at startup
     */
    boolean contains(String name);

    /**
     * The isolated client of the named group, sharing the gateway's TLS, proxy,
     * keep-alive and loop configuration but backed by its own bounded connection pool.
     *
     * @param name the group name
     * @return the group client
     * @throws IllegalStateException when the strategy did not build netty clients
     */
    HttpClient httpClientFor(String name);

    /**
     * The isolated WebClient of the named group, backed by the group's own client.
     *
     * @param name the group name
     * @return the group WebClient
     * @throws IllegalStateException when the strategy did not build WebClients
     */
    WebClient webClientFor(String name);

    /**
     * An empty registry for deployments without configured groups.
     *
     * @return the empty registry
     */
    static UpstreamPoolGroupRegistry empty() {
        return EmptyUpstreamPoolGroupRegistry.INSTANCE;
    }

    /**
     * Release the resources owned by this registry. The empty registry is a no-op;
     * a populated registry disposes each group connection provider exactly once.
     */
    @Override
    default void close() {
    }
}
