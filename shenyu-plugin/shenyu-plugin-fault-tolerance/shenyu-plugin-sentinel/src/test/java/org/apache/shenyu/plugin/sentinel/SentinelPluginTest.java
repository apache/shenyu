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

package org.apache.shenyu.plugin.sentinel;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.sentinel.fallback.SentinelFallbackHandler;
import org.apache.shenyu.plugin.sentinel.handler.SentinelRuleHandle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import reactor.test.publisher.TestPublisher;

import java.net.InetSocketAddress;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class SentinelPluginTest {

    private static final String QUOTA_HEADER = "X-Quota-Group";

    private SentinelPlugin sentinelPlugin;

    private ServerWebExchange exchange;

    private SelectorData selectorData;

    @Mock
    private SentinelRuleHandle sentinelRuleHandle;

    @Mock
    private SentinelFallbackHandler sentinelFallbackHandler;

    @Mock
    private ShenyuPluginChain chain;

    @BeforeEach
    public void setUp() {
        this.chain = mock(ShenyuPluginChain.class);
        this.selectorData = mock(SelectorData.class);
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .build());
        sentinelFallbackHandler = new SentinelFallbackHandler();
        sentinelRuleHandle = new SentinelRuleHandle();
        ShenyuContext context = mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, context);
        when(chain.execute(exchange)).thenReturn(Mono.empty());

        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        when(applicationContext.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());

        sentinelPlugin = new SentinelPlugin(sentinelFallbackHandler);
    }

    /**
     * Test Sentinel Flow.
     */
    @Test
    public void testSentinelPluginFlowException() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginFlowException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(0);
        sentinelHandle.setFlowRuleGrade(1);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleEnable(0);
        sentinelHandle.setDegradeRuleCount(1d);
        sentinelHandle.setDegradeRuleGrade(1);
        sentinelHandle.setDegradeRuleTimeWindow(10);
        sentinelHandle.setDegradeRuleMinRequestAmount(5);
        sentinelHandle.setDegradeRuleStatIntervals(10);
        sentinelHandle.setDegradeRuleSlowRatioThreshold(0.5d);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        sentinelRuleHandle.handlerRule(data);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription().verifyComplete();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    /**
     * Test chain.execute doOnSuccess return HttpStatus.OK.
     */
    @Test
    public void testSentinelPluginHttpStatusOK() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginNullPointException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(0);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleCount(2d);
        sentinelHandle.setDegradeRuleGrade(2);
        sentinelHandle.setDegradeRuleTimeWindow(5);
        sentinelHandle.setDegradeRuleMinRequestAmount(5);
        sentinelHandle.setDegradeRuleStatIntervals(10);
        sentinelHandle.setDegradeRuleSlowRatioThreshold(0.5d);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        sentinelRuleHandle.handlerRule(data);

        Mono mono = Mono.empty().doOnSuccess(v -> exchange.getResponse().setStatusCode(HttpStatus.OK));
        when(chain.execute(exchange)).thenReturn(mono);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data))
                .expectSubscription().verifyComplete();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    /**
     * Test chain.execute doOnSuccess return other status.
     */
    @Test
    public void testSentinelPluginNotHttpStatusOK() {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setName("testSentinelPluginNullPointException");
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleEnable(1);
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(0);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleCount(2d);
        sentinelHandle.setDegradeRuleGrade(2);
        sentinelHandle.setDegradeRuleTimeWindow(5);
        sentinelHandle.setDegradeRuleMinRequestAmount(5);
        sentinelHandle.setDegradeRuleStatIntervals(10);
        sentinelHandle.setDegradeRuleSlowRatioThreshold(0.5d);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        sentinelRuleHandle.handlerRule(data);

        Mono mono = Mono.empty().doOnSuccess(v -> exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS));
        when(chain.execute(exchange)).thenReturn(mono);
        StepVerifier.create(sentinelPlugin.doExecute(exchange, chain, selectorData, data)).expectComplete().verify();

        // remove rule
        sentinelRuleHandle.removeRule(data);
    }

    @Test
    public void testNamed() {
        final String result = sentinelPlugin.named();
        assertEquals(PluginEnum.SENTINEL.getName(), result);
    }

    @Test
    public void testParameterQuotasAreIndependentAndBlockBeforeDownstream() {
        RuleData rule = parameterRule("independent-quotas", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        sentinelRuleHandle.handlerRule(rule);
        try {
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("blue")));
            ServerWebExchange excess = quotaRequest("blue");
            assertEquals(HttpStatus.TOO_MANY_REQUESTS, invoke(rule, excess));
            verify(chain, never()).execute(excess);
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("green")));
        } finally {
            sentinelRuleHandle.removeRule(rule);
        }
    }

    @Test
    public void testConflictingSwitchesPreserveActiveParameterRule() {
        RuleData rule = parameterRule("invalid-update", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        sentinelRuleHandle.handlerRule(rule);
        try {
            SentinelHandle invalid = GsonUtils.getInstance().fromJson(rule.getHandle(), SentinelHandle.class);
            invalid.setFlowRuleEnable(0);
            rule.setHandle(GsonUtils.getGson().toJson(invalid));
            sentinelRuleHandle.handlerRule(rule);
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("blue")));
            assertEquals(HttpStatus.TOO_MANY_REQUESTS, invoke(rule, quotaRequest("blue")));
        } finally {
            sentinelRuleHandle.removeRule(rule);
        }
    }

    @Test
    public void testSameParameterValueHasIndependentQuotasAcrossRules() {
        RuleData first = parameterRule("first-rule", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        RuleData second = parameterRule("second-rule", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        sentinelRuleHandle.handlerRule(first);
        sentinelRuleHandle.handlerRule(second);
        try {
            assertEquals(HttpStatus.OK, invoke(first, quotaRequest("blue")));
            assertEquals(HttpStatus.TOO_MANY_REQUESTS, invoke(first, quotaRequest("blue")));
            assertEquals(HttpStatus.OK, invoke(second, quotaRequest("blue")));
        } finally {
            sentinelRuleHandle.removeRule(first);
            sentinelRuleHandle.removeRule(second);
        }
    }

    @Test
    public void testMissingAndAmbiguousHeaderShareASeparateBucket() {
        RuleData rule = parameterRule("missing-quota", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        sentinelRuleHandle.handlerRule(rule);
        try {
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest()));
            assertEquals(HttpStatus.TOO_MANY_REQUESTS, invoke(rule, quotaRequest("blue", "green")));
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("missing")));
        } finally {
            sentinelRuleHandle.removeRule(rule);
        }
    }

    @Test
    public void testAttributeSourceAndSkipMissingParameter() {
        RuleData rule = parameterRule("attribute-quota", "EXCHANGE_ATTRIBUTE", "quota.group", "SKIP");
        sentinelRuleHandle.handlerRule(rule);
        try {
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("header-only")));
            assertEquals(HttpStatus.OK, invoke(rule, quotaRequest("header-only")));
            ServerWebExchange first = quotaRequest("unrelated-header");
            first.getAttributes().put("quota.group", "green");
            assertEquals(HttpStatus.OK, invoke(rule, first));
            ServerWebExchange second = quotaRequest("different-header");
            second.getAttributes().put("quota.group", "green");
            assertEquals(HttpStatus.TOO_MANY_REQUESTS, invoke(rule, second));
        } finally {
            sentinelRuleHandle.removeRule(rule);
        }
    }

    @Test
    public void testParameterFlowPropagatesCancellationToDownstream() {
        RuleData rule = parameterRule("cancel-quota", "HEADER", QUOTA_HEADER, "SHARED_BUCKET");
        ServerWebExchange request = quotaRequest("green");
        TestPublisher<Void> downstream = TestPublisher.create();
        when(chain.execute(request)).thenReturn(downstream.mono());
        sentinelRuleHandle.handlerRule(rule);
        try {
            StepVerifier.create(sentinelPlugin.doExecute(request, chain, selectorData, rule))
                    .then(downstream::assertWasSubscribed).thenCancel().verify();
            downstream.assertCancelled();
        } finally {
            sentinelRuleHandle.removeRule(rule);
        }
    }

    private RuleData parameterRule(final String id, final String source, final String key, final String missingPolicy) {
        RuleData rule = new RuleData();
        rule.setSelectorId("sentinel");
        rule.setId(id);
        SentinelHandle handle = new SentinelHandle();
        handle.setFlowRuleEnable(1);
        handle.setFlowRuleCount(1);
        handle.setDegradeRuleEnable(0);
        handle.setFallbackUri("");
        handle.setParamFlowEnable(1);
        handle.setParamFlowSource(source);
        handle.setParamFlowKey(key);
        handle.setParamFlowMissingKeyPolicy(missingPolicy);
        rule.setHandle(GsonUtils.getGson().toJson(handle));
        return rule;
    }

    private ServerWebExchange quotaRequest(final String... values) {
        ServerWebExchange request = MockServerWebExchange.from(MockServerHttpRequest.get("/reports").header(QUOTA_HEADER, values));
        request.getAttributes().put(Constants.CONTEXT, mock(ShenyuContext.class));
        return request;
    }

    private HttpStatus invoke(final RuleData rule, final ServerWebExchange request) {
        when(chain.execute(request)).thenReturn(Mono.fromRunnable(() -> request.getResponse().setStatusCode(HttpStatus.OK)));
        StepVerifier.create(sentinelPlugin.doExecute(request, chain, selectorData, rule)).verifyComplete();
        return HttpStatus.valueOf(request.getResponse().getStatusCode().value());
    }

    @Test
    public void testGetOrder() {
        final int result = sentinelPlugin.getOrder();
        assertEquals(PluginEnum.SENTINEL.getCode(), result);
    }
}
