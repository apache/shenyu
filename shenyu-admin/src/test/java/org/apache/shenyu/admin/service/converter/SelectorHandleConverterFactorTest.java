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

package org.apache.shenyu.admin.service.converter;

import org.apache.shenyu.admin.config.ShenyuAdminConfiguration;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link SelectorHandleConverterFactor}.
 */
class SelectorHandleConverterFactorTest {

    private SelectorHandleConverterFactor factor;

    private DivideSelectorHandleConverter divideConverter;

    private DubboSelectorHandleConverter dubboConverter;

    private GrpcSelectorHandleConverter grpcConverter;

    @BeforeEach
    void setUp() {
        divideConverter = new DivideSelectorHandleConverter();
        dubboConverter = new DubboSelectorHandleConverter();
        grpcConverter = new GrpcSelectorHandleConverter();

        Map<String, SelectorHandleConverter> maps = new HashMap<>();
        maps.put("divide", divideConverter);
        maps.put("dubbo", dubboConverter);
        maps.put("grpc", grpcConverter);

        factor = new SelectorHandleConverterFactor(maps);
    }

    @ParameterizedTest
    @CsvSource({
        "divide,divide",
        "dubbo,dubbo",
        "grpc,grpc"
    })
    void testNewInstanceWithValidPlugins(final String pluginName, final String expectedPluginName) {
        SelectorHandleConverter converter = factor.newInstance(pluginName);
        assertNotNull(converter);
        assertEquals(expectedPluginName, converter.pluginName());

        if ("divide".equals(pluginName)) {
            assertSame(divideConverter, converter);
        } else if ("dubbo".equals(pluginName)) {
            assertSame(dubboConverter, converter);
        } else if ("grpc".equals(pluginName)) {
            assertSame(grpcConverter, converter);
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"nonexistent", "unknown", "invalid"})
    void testNewInstanceWithInvalidPlugins(final String pluginName) {
        SelectorHandleConverter converter = factor.newInstance(pluginName);
        assertNull(converter);
    }

    @Test
    void testWebSocketHealthUpdateRetainsConfigurationAcrossRecovery() {
        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext("org.apache.shenyu.admin.service.converter")) {
            SelectorHandleConverterFactor discovered = new ShenyuAdminConfiguration().selectorHandleConverterFactor(
                    List.copyOf(context.getBeansOfType(SelectorHandleConverter.class).values()));
            SelectorHandleConverter converter = discovered.newInstance(PluginEnum.WEB_SOCKET.getName());
            assertNotNull(converter, "Admin must be able to reconcile WebSocket upstreams after a health check");
            String previous = """
                    [
                      {"url":"127.0.0.1:8868","upstreamUrl":"127.0.0.1:8868","protocol":"ws://","status":true,"weight":75},
                      {"url":"127.0.0.1:8870","upstreamUrl":"127.0.0.1:8870","protocol":"ws://","status":true,"weight":75,"warmup":7}
                    ]
                    """;
            CommonUpstream healthy = new CommonUpstream("ws://", "localhost", "127.0.0.1:8868", true, 1L);
            String reconciled = converter.handler(previous, List.of(healthy));
            List<WebSocketUpstream> remaining = GsonUtils.getInstance().fromList(reconciled, WebSocketUpstream.class);
            assertEquals(2, remaining.size());
            assertEquals("127.0.0.1:8868", remaining.get(0).getUrl());
            assertEquals("ws://", remaining.get(0).getProtocol());
            assertEquals(75, remaining.get(0).getWeight());
            assertTrue(remaining.get(0).isStatus());
            assertEquals("127.0.0.1:8870", remaining.get(1).getUrl());
            assertFalse(remaining.get(1).isStatus());

            CommonUpstream recovered = new CommonUpstream("ws://", "localhost", "127.0.0.1:8870", true, 1L);
            CommonUpstream added = new CommonUpstream("ws://", "localhost", "127.0.0.1:8872", true, 1L);
            List<WebSocketUpstream> updated = GsonUtils.getInstance()
                    .fromList(converter.handler(reconciled, List.of(healthy, recovered, added)), WebSocketUpstream.class);
            assertEquals(3, updated.size());
            assertTrue(updated.get(1).isStatus());
            assertEquals(75, updated.get(1).getWeight());
            assertEquals(7, updated.get(1).getWarmup());
            assertEquals("127.0.0.1:8872", updated.get(2).getUrl());
            assertTrue(updated.get(2).isStatus());
            assertEquals(50, updated.get(2).getWeight());
            assertEquals(Constants.WARMUP_TIME, updated.get(2).getWarmup());
        }
    }

    @Test
    void testFactorWithEmptyMap() {
        SelectorHandleConverterFactor emptyFactor = new SelectorHandleConverterFactor(new HashMap<>());
        SelectorHandleConverter converter = emptyFactor.newInstance("divide");
        assertNull(converter);
    }
}
