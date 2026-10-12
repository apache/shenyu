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

package org.apache.shenyu.sdk.spring.proxy;

import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.spring.support.SpringMvcContract;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Object methods called on a ShenYu client proxy.
 */
public class ShenyuClientObjectMethodsTest {

    private AnnotationConfigApplicationContext context;

    private ShenyuSdkClient sdkClient;

    private AbstractProxyTest.InvocationClient client;

    @BeforeEach
    public void setUp() {
        sdkClient = mock(ShenyuSdkClient.class);
        context = new AnnotationConfigApplicationContext();
        context.register(AbstractProxyTest.TestConfig.class);
        context.register(SpringMvcContract.class);
        context.registerBean("shenyuSdkClient", ShenyuSdkClient.class, () -> sdkClient);
        context.refresh();
        client = context.getBean(AbstractProxyTest.InvocationClient.class);
    }

    @AfterEach
    public void tearDown() {
        context.close();
    }

    @Test
    public void testToString() throws IOException {
        final String value = client.toString();
        assertTrue(value.contains(AbstractProxyTest.InvocationClient.class.getName()), value);
        verify(sdkClient, never()).execute(any());
    }

    @Test
    public void testHashCodeAndEquals() throws IOException {
        assertEquals(client.hashCode(), client.hashCode());
        assertEquals(client, client);
        assertNotEquals(client, null);
        assertNotEquals(client, new Object());
        final Set<Object> set = new HashSet<>();
        set.add(client);
        assertTrue(set.contains(client));
        verify(sdkClient, never()).execute(any());
    }
}
