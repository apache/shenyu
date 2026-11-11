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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProtocolResponseInspector;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocol;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderCapabilities;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderProtocolBinding;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProviderRequestTransformer;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProvider;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProxyExtensionContractTest {

    private static final String PROTOCOL = "fixture-protocol";

    @Test
    void testNewNativeProtocolAndProviderDoNotChangeEngineFlow() {
        final AtomicBoolean responseInspected = new AtomicBoolean();
        final AtomicBoolean attemptObserved = new AtomicBoolean();
        final AtomicReference<AiUpstreamRequest> transported = new AtomicReference<>();
        final AtomicReference<String> downstreamBody = new AtomicReference<>();
        final AiProxyEngine engine = AiProxyEngine.builder(
                new AiProxyProtocolFactory(List.of(protocol(responseInspected))),
                new AiProxyProviderFactory(List.of(provider())),
                new AiProtocolRouteResolver(new AiProtocolConverterFactory(List.of())),
                request -> {
                    transported.set(request);
                    return Mono.just(new AiUpstreamResponse(200, Map.of(),
                            Flux.just("fixture-response".getBytes(StandardCharsets.UTF_8))));
                })
                .attemptListener(event -> attemptObserved.set(true))
                .build();
        final AiCommonConfig config = new AiCommonConfig();
        config.setProvider("fixture-provider");
        config.setProtocol(PROTOCOL);

        StepVerifier.create(engine.execute("{\"fixture_input\":\"hello\",\"model\":\"fixture-model\"}",
                        config, Optional.empty(), response -> response.getBody()
                                .doOnNext(bytes -> downstreamBody.set(new String(bytes, StandardCharsets.UTF_8))).then()))
                .verifyComplete();

        assertEquals(URI.create("https://fixture.invalid/native"), transported.get().getUri());
        assertEquals("fixture-response", downstreamBody.get());
        assertTrue(responseInspected.get());
        assertTrue(attemptObserved.get());
    }

    private AiProxyProtocol protocol(final AtomicBoolean responseInspected) {
        return new AiProxyProtocol() {
            @Override
            public String name() {
                return PROTOCOL;
            }

            @Override
            public boolean matches(final JsonNode payload) {
                return payload.has("fixture_input");
            }

            @Override
            public ShenyuAiRequest decodeRequest(final String rawBody, final ObjectNode payload) {
                final ShenyuAiRequest request = new ShenyuAiRequest();
                request.setRawBody(rawBody);
                request.setRawPayload(payload);
                request.setProtocol(PROTOCOL);
                request.setOperation(ShenyuAiRequest.Operation.CHAT_COMPLETION);
                request.setModel(payload.path("model").asText());
                return request;
            }

            @Override
            public AiProtocolResponseInspector responseInspector() {
                return (response, observation, stream) -> {
                    responseInspected.set(true);
                    return response;
                };
            }
        };
    }

    private AiProxyProvider provider() {
        return new AiProxyProvider() {
            @Override
            public String name() {
                return "fixture-provider";
            }

            @Override
            public AiProviderCapabilities capabilities() {
                return new AiProviderCapabilities(Map.of(PROTOCOL,
                        new AiProviderProtocolBinding("/native", AiProviderRequestTransformer.identity())),
                        EnumSet.of(ShenyuAiRequest.Operation.CHAT_COMPLETION), Set.of(),
                        EnumSet.of(AiProviderCapabilities.ReplaySafety.SAFE));
            }

            @Override
            public AiUpstreamRequest buildRequest(final ShenyuAiRequest request, final AiCommonConfig config) {
                final AiUpstreamRequest upstream = new AiUpstreamRequest();
                upstream.setUri(URI.create("https://fixture.invalid/native"));
                upstream.setMethod("POST");
                upstream.setHeaders(Map.of("Content-Type", List.of("application/json")));
                upstream.setBody(request.getRawBody().getBytes(StandardCharsets.UTF_8));
                return upstream;
            }
        };
    }
}
