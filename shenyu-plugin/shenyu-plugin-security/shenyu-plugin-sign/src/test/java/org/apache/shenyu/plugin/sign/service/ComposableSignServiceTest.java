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

package org.apache.shenyu.plugin.sign.service;

import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.sign.extractor.DefaultExtractor;
import org.apache.shenyu.plugin.sign.provider.DefaultSignProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test for ComposableSignService#skipSignExchange.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ComposableSignServiceTest {

    private ComposableSignService signService;

    @BeforeEach
    public void setUp() {
        this.signService = new ComposableSignService(new DefaultExtractor(), new DefaultSignProvider());
    }

    private boolean skipSignExchange(final String module, final String rpcType) {
        ShenyuContext context = new ShenyuContext();
        context.setModule(module);
        context.setRpcType(rpcType);
        return Boolean.TRUE.equals(ReflectionTestUtils.invokeMethod(this.signService, "skipSignExchange", context));
    }

    @Test
    public void testSkipSignExchangeForSupportedPlugins() {
        assertTrue(skipSignExchange("divide-http", "http"));
        assertTrue(skipSignExchange("springCloud-http", "http"));
        assertTrue(skipSignExchange("websocket-ws", "ws"));
    }

    @Test
    public void testSkipSignExchangeWithMismatchedRpcType() {
        assertFalse(skipSignExchange("divide-http", "grpc"));
        assertFalse(skipSignExchange("springCloud-grpc", "http"));
    }

    @Test
    public void testSkipSignExchangeForUnsupportedPlugin() {
        assertFalse(skipSignExchange("dubbo-http", "http"));
        assertFalse(skipSignExchange("grpc-http", "http"));
    }

    @Test
    public void testSkipSignExchangeWithMalformedModule() {
        assertFalse(skipSignExchange("http", "http"));
        assertFalse(skipSignExchange("divide-", ""));
        assertFalse(skipSignExchange("", "http"));
        assertFalse(skipSignExchange("divide-springCloud-http", "http"));
    }
}
