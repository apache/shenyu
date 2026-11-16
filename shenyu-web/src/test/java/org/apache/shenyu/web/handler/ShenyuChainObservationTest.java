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

package org.apache.shenyu.web.handler;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.match.SelectorRuleMatchObservation;
import org.apache.shenyu.plugin.base.match.SelectorRuleMatchObserverHolder;
import org.apache.shenyu.web.loader.ShenyuLoaderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Exercises the matching-decision observation through the real plugin chain
 * ({@link ShenyuWebHandler} and its default chain): outer skip stays at the chain
 * boundary, a plugin outside {@link AbstractShenyuPlugin} is never observed, and only a
 * real matching decision of a base-plugin emits exactly one event.
 */
public final class ShenyuChainObservationTest {

    private final List<SelectorRuleMatchObservation> events = new java.util.ArrayList<>();

    private ShenyuWebHandler handler;

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = Mockito.mock(ConfigurableApplicationContext.class);
        Mockito.when(context.getBean(ShenyuConfig.class)).thenReturn(new ShenyuConfig());
        SpringBeanUtils.getInstance().setApplicationContext(context);
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().name("SHENYU").enabled(true).build());
        BaseDataCache.getInstance().cacheSelectData(SelectorData.builder().id("s1").pluginName("SHENYU")
                .enabled(true).matchRestful(false).logged(true)
                .type(SelectorTypeEnum.FULL_FLOW.getCode()).continued(true).sort(1).build());
        BaseDataCache.getInstance().cacheRuleData(RuleData.builder().id("r1").pluginName("SHENYU").selectorId("s1")
                .enabled(true).logged(true).matchRestful(false).sort(1).build());
        SelectorRuleMatchObserverHolder.register(events::add);
        handler = new ShenyuWebHandler(List.of(new SkippingPlugin(), new PlainPlugin(), new MatchingPlugin()),
                Mockito.mock(ShenyuLoaderService.class), new ShenyuConfig());
    }

    @AfterEach
    public void cleanUp() {
        SelectorRuleMatchObserverHolder.unregister();
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
    }

    @Test
    public void realChainEmitsOnlyTheBasePluginDecision() {
        MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/any").build());
        exchange.getAttributes().put(Constants.CONTEXT, Mockito.mock(ShenyuContext.class));
        handler.handle(exchange).block();

        // the skipped plugin never reaches execute, the plain plugin has no observation
        // point, and only the matching base plugin reports its real decision once
        assertEquals(1, events.size(), "expected exactly one chain-observed decision: " + describe(events));
        assertEquals(SelectorRuleMatchObservation.Decision.FULL_FLOW_RULE, events.get(0).getDecision());
        assertEquals("SHENYU", events.get(0).getPluginName());
        assertEquals("r1", events.get(0).getRuleId());
    }

    private static String describe(final List<SelectorRuleMatchObservation> observations) {
        return observations.stream()
                .map(observation -> observation.getPluginName() + ":" + observation.getDecision())
                .collect(Collectors.joining(", "));
    }

    private static final class SkippingPlugin extends AbstractShenyuPlugin {

        @Override
        public boolean skip(final ServerWebExchange exchange) {
            return true;
        }

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange serverExchange, final ShenyuPluginChain pluginChain,
                                       final SelectorData selector, final RuleData rule) {
            return pluginChain.execute(serverExchange);
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "SKIPPING";
        }
    }

    private static final class PlainPlugin implements ShenyuPlugin {

        @Override
        public Mono<Void> execute(final ServerWebExchange serverExchange, final ShenyuPluginChain pluginChain) {
            return pluginChain.execute(serverExchange);
        }

        @Override
        public int getOrder() {
            return 10;
        }

        @Override
        public String named() {
            return "PLAIN";
        }
    }

    private static final class MatchingPlugin extends AbstractShenyuPlugin {

        @Override
        protected Mono<Void> doExecute(final ServerWebExchange serverExchange, final ShenyuPluginChain pluginChain,
                                       final SelectorData selector, final RuleData rule) {
            return pluginChain.execute(serverExchange);
        }

        @Override
        public int getOrder() {
            return 20;
        }

        @Override
        public String named() {
            return "SHENYU";
        }
    }
}
