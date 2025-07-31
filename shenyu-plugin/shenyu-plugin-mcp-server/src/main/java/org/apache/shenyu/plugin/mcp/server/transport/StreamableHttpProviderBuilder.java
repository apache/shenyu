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

package org.apache.shenyu.plugin.mcp.server.transport;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.util.Assert;

/**
 * Builder class for constructing instances of ShenyuStreamableHttpServerTransportProvider.
 * This builder provides a fluent interface for configuring transport provider instances
 * with proper validation and default values.
 * Usage Example:
 * ShenyuStreamableHttpServerTransportProvider provider =
 * ShenyuStreamableHttpServerTransportProvider.builder()
 * .objectMapper(new ObjectMapper())
 * .endpoint("/mcp")
 * .build();
 */
public class StreamableHttpProviderBuilder {

    /**
     * Default unified endpoint path for Streamable HTTP protocol.
     */
    private static final String DEFAULT_ENDPOINT = "/mcp";

    private ObjectMapper objectMapper;

    private String endpoint = DEFAULT_ENDPOINT;

    /**
     * Sets the ObjectMapper for JSON serialization/deserialization.
     *
     * @param objectMapper the ObjectMapper instance (required)
     * @return this builder for method chaining
     * @throws IllegalArgumentException if objectMapper is null
     */
    public StreamableHttpProviderBuilder objectMapper(final ObjectMapper objectMapper) {
        Assert.notNull(objectMapper, "ObjectMapper must not be null");
        this.objectMapper = objectMapper;
        return this;
    }

    /**
     * Sets the endpoint path for the transport provider.
     *
     * @param endpoint the endpoint path (defaults to "/mcp" if not specified)
     * @return this builder for method chaining
     * @throws IllegalArgumentException if endpoint is null
     */
    public StreamableHttpProviderBuilder endpoint(final String endpoint) {
        Assert.notNull(endpoint, "Endpoint must not be null");
        this.endpoint = endpoint;
        return this;
    }

    /**
     * Builds a new ShenyuStreamableHttpServerTransportProvider instance.
     *
     * @return the configured transport provider
     * @throws IllegalStateException if required configuration is missing
     */
    public ShenyuStreamableHttpServerTransportProvider build() {
        Assert.notNull(objectMapper, "ObjectMapper must be configured");
        return new ShenyuStreamableHttpServerTransportProvider(objectMapper, endpoint);
    }
}