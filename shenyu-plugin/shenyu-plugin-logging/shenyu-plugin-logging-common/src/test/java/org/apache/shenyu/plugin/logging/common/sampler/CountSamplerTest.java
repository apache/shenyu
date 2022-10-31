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

package org.apache.shenyu.plugin.logging.common.sampler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.util.BitSet;

/**
 * The Test Case For CountSampler.
 */
public class CountSamplerTest {

    private CountSampler countSampler;

    private ServerHttpRequest request;
    
    @BeforeEach
    public void setUp() {
        this.countSampler = new CountSampler(1);
        MockServerHttpRequest request = MockServerHttpRequest
                .get("localhost")
                .remoteAddress(new InetSocketAddress(8090))
                .header("X-source", "mock test")
                .queryParam("queryParam", "Hello,World")
                .build();
        ServerWebExchange exchange = Mockito.spy(MockServerWebExchange.from(request));
        ShenyuContext shenyuContext = Mockito.mock(ShenyuContext.class);
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        this.request = exchange.getRequest();
    }

    @Test
    public void testIsSampled() {
        Assertions.assertTrue(countSampler.isSampled(request));
    }

    @Test
    public void testMod() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method method = countSampler.getClass().getDeclaredMethod("mod", int.class);
        method.setAccessible(true);
        int res = (int) method.invoke(countSampler, 1);
        Assertions.assertEquals(res, 1);
    }

    @Test
    public void testGenRandomBitSet() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method method = countSampler.getClass().getDeclaredMethod("genRandomBitSet", int.class, int.class);
        method.setAccessible(true);
        BitSet bitSet = (BitSet) method.invoke(countSampler, 1, 1);
        BitSet res = new BitSet(1);
        res.set(0);
        Assertions.assertEquals(bitSet, res);
    }

    @Test
    public void testCreate() {
        Assertions.assertEquals(CountSampler.create(""), Sampler.ALWAYS_SAMPLE);
        Assertions.assertEquals(CountSampler.create("0"), Sampler.NEVER_SAMPLE);
        Assertions.assertEquals(CountSampler.create("1"), Sampler.ALWAYS_SAMPLE);
        Assertions.assertEquals(CountSampler.create("0.5").getClass(), CountSampler.class);
    }
}
