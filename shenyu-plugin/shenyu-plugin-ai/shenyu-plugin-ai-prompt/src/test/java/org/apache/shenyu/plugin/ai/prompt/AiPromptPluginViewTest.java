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

package org.apache.shenyu.plugin.ai.prompt;

import org.apache.shenyu.common.ai.AiNormalizedRequestView;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiPromptConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.buffer.DefaultDataBufferFactory;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.ServerRequest;
import reactor.core.publisher.Flux;
import reactor.test.StepVerifier;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The prompt plugin publishes the wire view once and, when decoration really changes
 * the body, the effective view with a bumped generation; a consumer that inspected an
 * older generation is observable as superseded.
 */
class AiPromptPluginViewTest {

    private static final String BODY = "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private AiPromptPlugin plugin;

    private AiPromptConfig previousConfig;

    @BeforeEach
    void setUp() {
        plugin = new AiPromptPlugin(ServerCodecConfigurer.create().getReaders());
        previousConfig = Singleton.INST.get(AiPromptConfig.class);
    }

    @AfterEach
    void tearDown() throws Exception {
        Field singlesField = Singleton.class.getDeclaredField("SINGLES");
        singlesField.setAccessible(true);
        Map<?, ?> singles = (Map<?, ?>) singlesField.get(null);
        if (Objects.isNull(previousConfig)) {
            singles.remove(AiPromptConfig.class.getName());
        } else {
            Singleton.INST.single(AiPromptConfig.class, previousConfig);
        }
    }

    @Test
    void decorationPublishesTheWireAndEffectiveGenerations() {
        MockServerWebExchange exchange = promptExchange();
        Singleton.INST.single(AiPromptConfig.class, config("system", "be brief", null, null));

        StepVerifier.create(plugin.doExecute(exchange, passthroughChain(), null, rule())).verifyComplete();

        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(2L, view.generation());
        assertEquals(AiNormalizedRequestView.ViewKind.EFFECTIVE, view.viewKind());
        assertEquals(PluginEnum.AI_PROMPT.getName(), view.mutator());
        assertEquals(2, view.messages().size());
        assertEquals("system", view.messages().get(0).role());
        assertEquals("be brief", view.messages().get(0).textPrefix());
        assertEquals("user", view.messages().get(1).role());
    }

    @Test
    void passThroughWithoutDecorationKeepsTheWireGeneration() {
        MockServerWebExchange exchange = promptExchange();
        Singleton.INST.single(AiPromptConfig.class, new AiPromptConfig());

        StepVerifier.create(plugin.doExecute(exchange, passthroughChain(), null, rule())).verifyComplete();

        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(1L, view.generation());
        assertEquals(AiNormalizedRequestView.ViewKind.WIRE, view.viewKind());
        assertEquals(1, view.messages().size());
        assertEquals("user", view.messages().get(0).role());
        assertEquals("hello", view.messages().get(0).textPrefix());
    }

    @Test
    void aConsumerOfThePreDecorationGenerationIsSuperseded() {
        MockServerWebExchange exchange = promptExchange();
        AiRequestViewOperations.ensurePublished(exchange, BODY, null);
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, "preTemplateConsumer");
        Singleton.INST.single(AiPromptConfig.class, config("system", "be brief", null, null));

        StepVerifier.create(plugin.doExecute(exchange, passthroughChain(), null, rule())).verifyComplete();
        AiRequestViewOperations.snapshotAndRecordInspection(exchange, PluginEnum.AI_TOKEN_LIMITER.getName());

        List<AiRequestViewOperations.Inspection> report = AiRequestViewOperations.inspectionReport(exchange);
        assertEquals(2, report.size());
        assertEquals("preTemplateConsumer", report.get(0).consumer());
        assertFalse(report.get(0).current());
        assertEquals(PluginEnum.AI_TOKEN_LIMITER.getName(), report.get(1).consumer());
        assertTrue(report.get(1).current());
    }

    @Test
    void aBodyStreamedInSplitsStillYieldsOneView() {
        DefaultDataBufferFactory buffers = DefaultDataBufferFactory.sharedInstance;
        String firstPart = BODY.substring(0, 30);
        String secondPart = BODY.substring(30);
        MockServerHttpRequest request = MockServerHttpRequest.post("/ai/prompt")
                .contentType(MediaType.APPLICATION_JSON)
                .body(Flux.just(buffers.wrap(firstPart.getBytes()), buffers.wrap(secondPart.getBytes())));
        MockServerWebExchange exchange = MockServerWebExchange.from(request);
        Singleton.INST.single(AiPromptConfig.class, new AiPromptConfig());

        StepVerifier.create(plugin.doExecute(exchange, passthroughChain(), null, rule())).verifyComplete();

        AiNormalizedRequestView view = AiRequestViewOperations.snapshot(exchange).orElseThrow();
        assertEquals(1L, view.generation());
        assertEquals(1, view.messages().size());
        assertEquals("hello", view.messages().get(0).textPrefix());
    }

    private MockServerWebExchange promptExchange() {
        return MockServerWebExchange.from(MockServerHttpRequest.post("/ai/prompt")
                .contentType(MediaType.APPLICATION_JSON)
                .body(BODY));
    }

    private AiPromptConfig config(final String preRole, final String prepend, final String postRole, final String append) {
        AiPromptConfig config = new AiPromptConfig();
        config.setPreRole(preRole);
        config.setPrepend(prepend);
        config.setPostRole(postRole);
        config.setAppend(append);
        return config;
    }

    private RuleData rule() {
        return RuleData.builder().selectorId("prompt-view").id("view-rule").build();
    }

    private ShenyuPluginChain passthroughChain() {
        return exchange -> ServerRequest.create(exchange, ServerCodecConfigurer.create().getReaders())
                .bodyToMono(String.class).then();
    }
}
