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

package org.apache.shenyu.plugin.ai.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.api.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.api.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiResponse;
import org.apache.shenyu.plugin.ai.api.model.ShenyuAiStreamEvent;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiProtocol;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiProvider;
import org.apache.shenyu.plugin.ai.api.spi.ShenyuAiTransport;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;
import org.apache.shenyu.spi.SPI;
import org.junit.jupiter.api.Test;

import java.util.Set;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Tests the shared AI API serialization and extension contracts.
 */
public final class ShenyuAiApiContractTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    public void testRequestRetainsUnknownProtocolFields() throws Exception {
        JsonNode payload = objectMapper.readTree("{\"model\":\"m1\",\"vendor_option\":{\"mode\":\"fast\"}}");
        ShenyuAiRequest request = new ShenyuAiRequest("chat", "m1", false, null, payload);

        ShenyuAiRequest restored = objectMapper.readValue(objectMapper.writeValueAsBytes(request), ShenyuAiRequest.class);

        assertEquals("fast", restored.payload().path("vendor_option").path("mode").asText());
        assertEquals("m1", restored.model());
    }

    @Test
    public void testExtensionContractsAreShenyuSpiInterfaces() {
        assertNotNull(ShenyuAiProtocol.class.getAnnotation(SPI.class));
        assertNotNull(ShenyuAiProvider.class.getAnnotation(SPI.class));
        assertNotNull(ShenyuAiTransport.class.getAnnotation(SPI.class));
    }

    @Test
    public void testExtensionContractsLoadRegisteredImplementations() {
        assertEquals("contractTestProtocol", ExtensionLoader.getExtensionLoader(ShenyuAiProtocol.class)
                .getJoin("contractTest").getName());
        assertEquals("contractTestProvider", ExtensionLoader.getExtensionLoader(ShenyuAiProvider.class)
                .getJoin("contractTest").getName());
        assertEquals("contractTestTransport", ExtensionLoader.getExtensionLoader(ShenyuAiTransport.class)
                .getJoin("contractTest").getName());
    }

    /** Test protocol SPI implementation. */
    @Join
    public static final class TestShenyuAiProtocol implements ShenyuAiProtocol {

        @Override
        public String getName() {
            return "contractTestProtocol";
        }

        @Override
        public ShenyuAiRequest decodeRequest(final JsonNode payload) {
            return new ShenyuAiRequest("test", null, false, null, payload);
        }

        @Override
        public JsonNode encodeResponse(final ShenyuAiResponse response) {
            return response.payload();
        }

        @Override
        public Flux<JsonNode> encodeStream(final Flux<ShenyuAiStreamEvent> events) {
            return Flux.empty();
        }
    }

    /** Test provider SPI implementation. */
    @Join
    public static final class TestShenyuAiProvider implements ShenyuAiProvider {

        @Override
        public String getName() {
            return "contractTestProvider";
        }

        @Override
        public Set<String> getSupportedProtocols() {
            return Set.of("test");
        }

        @Override
        public AiUpstreamRequest createRequest(final ShenyuAiRequest request) {
            return null;
        }

        @Override
        public Mono<ShenyuAiResponse> decodeResponse(final AiUpstreamResponse response) {
            return Mono.empty();
        }

        @Override
        public Flux<ShenyuAiStreamEvent> decodeStream(final AiUpstreamResponse response) {
            return Flux.empty();
        }
    }

    /** Test transport SPI implementation. */
    @Join
    public static final class TestShenyuAiTransport implements ShenyuAiTransport {

        @Override
        public String getName() {
            return "contractTestTransport";
        }

        @Override
        public Mono<AiUpstreamResponse> execute(final AiUpstreamRequest request) {
            return Mono.empty();
        }
    }
}
