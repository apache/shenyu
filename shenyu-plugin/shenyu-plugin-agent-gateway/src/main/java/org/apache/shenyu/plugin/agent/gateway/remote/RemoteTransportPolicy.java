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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/** Fixed-IP transport and opaque metadata boundary; does not support DNS-based targets. */
public final class RemoteTransportPolicy {

    private RemoteTransportPolicy() { }

    /**
     * direct Only.
     * @return operation result
     */
    public static ProxySelector directOnly() {
        return new ProxySelector() {
            @Override
            public List<Proxy> select(final URI uri) {
                return List.of(Proxy.NO_PROXY);
            }

            @Override
            public void connectFailed(final URI uri, final SocketAddress address, final IOException error) { }
        };
    }

    /**
     * endpoint.
     * @param uri trusted uri value
     */
    public static void endpoint(final URI uri) {
        // No DNS: JDK connects to the configured address and validates HTTPS against its IP SAN.
        org.apache.shenyu.common.dto.AgentGatewayAggregationConfig.validateEndpoint(uri);
    }

    /**
     * session.
     * @param values trusted values value
     * @return operation result
     */
    public static String session(final List<String> values) {
        // Stateless upstream is not forced to invent a session.
        if (values.isEmpty()) {
            return null;
        }
        if (values.size() != 1 || !values.get(0).matches("[A-Za-z0-9._~-]{1,128}")) {
            throw new IllegalArgumentException("Invalid upstream session header");
        }
        return values.get(0);
    }

    /**
     * subject.
     * @param value trusted value value
     * @return operation result
     */
    public static String subject(final Object value) {
        if (java.util.Objects.isNull(value)) {
            return "null";
        }
        if (!(value instanceof String text) || !text.matches("[A-Za-z0-9._@:/-]{1,128}")) {
            throw new IllegalArgumentException("Invalid trusted diagnostic subject");
        }
        return text;
    }

    /**
     * call Metadata.
     * @param request trusted request value
     */
    public static void callMetadata(final ObjectNode request) {
        JsonNode meta = request.get("_meta");
        if (java.util.Objects.nonNull(meta) && !meta.isNull() && (!meta.isObject() || !meta.isEmpty())) {
            // No silent filtering/fallback: this narrow managed has no delegated metadata/progress contract.
            throw new IllegalArgumentException("Outbound request metadata is not supported");
        }
    }

    /**
     * result Metadata.
     * @param result trusted result value
     */
    public static void resultMetadata(final ObjectNode result) {
        JsonNode meta = result.get("_meta");
        if (java.util.Objects.isNull(meta)) {
            return;
        }
        if (!meta.isObject()) {
            throw new IllegalArgumentException("Invalid upstream result metadata");
        }
        inspect(meta, 0, new AtomicInteger());
        // Validation only. Unknown fields/values stay opaque data; never become credentials, routing or headers.
    }

    private static void inspect(final JsonNode node, final int depth, final AtomicInteger count) {
        if (depth > 8 || count.incrementAndGet() > 128 || (node.isTextual() && node.textValue().length() > 2048)) {
            throw new IllegalArgumentException("Upstream metadata budget exceeded");
        }
        if (node.isObject()) {
            var fields = node.fields();
            while (fields.hasNext()) {
                var field = fields.next();
                if (
                    field.getKey().isEmpty()
                    || field.getKey().length() > 128
                        || field
                        .getKey()
                        .chars()
                        .anyMatch(x -> x < 32 || x == 127)
                ) {
                    throw new IllegalArgumentException("Invalid upstream metadata key");
                }
                inspect(field.getValue(), depth + 1, count);
            }
        } else if (node.isArray()) {
            for (JsonNode value : node) {
                inspect(value, depth + 1, count);
            }
        }
    }
}
