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

package org.apache.shenyu.plugin.ai.token.limiter;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The token limiter is an inspection-only view consumer: it records the generation it
 * acted on when a view was published before this stage, and records nothing (keeping
 * its legacy behavior byte for byte) when no publisher ran.
 */
class AiTokenLimiterViewInspectionTest {

    private static final String CHAT_BODY = "{\"model\":\"gpt-4\",\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}";

    private final AiTokenLimiterPlugin plugin = new AiTokenLimiterPlugin();

    private final RuleData rule = RuleData.builder().selectorId("token-view").id("inspection").build();

    private final ShenyuPluginChain passthrough = exchange -> Mono.empty();

    @Test
    void theStageRecordsTheGenerationItActedOn() {
        MockServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("/ai/token").build());
        AiRequestViewOperations.ensurePublished(exchange, CHAT_BODY, null);

        StepVerifier.create(plugin.doExecute(exchange, passthrough, null, rule)).verifyComplete();

        List<AiRequestViewOperations.Inspection> report = AiRequestViewOperations.inspectionReport(exchange);
        assertEquals(1, report.size());
        assertEquals(PluginEnum.AI_TOKEN_LIMITER.getName(), report.get(0).consumer());
        assertEquals(1L, report.get(0).generation());
        assertTrue(report.get(0).current());
    }

    @Test
    void withoutAPublishedViewNothingIsRecorded() {
        MockServerWebExchange exchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("/ai/token").build());

        StepVerifier.create(plugin.doExecute(exchange, passthrough, null, rule)).verifyComplete();

        assertTrue(AiRequestViewOperations.inspectionReport(exchange).isEmpty());
        assertTrue(AiRequestViewOperations.snapshot(exchange).isEmpty());
    }
}
