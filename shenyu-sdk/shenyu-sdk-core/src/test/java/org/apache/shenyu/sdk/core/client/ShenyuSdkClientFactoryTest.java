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

package org.apache.shenyu.sdk.core.client;

import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test for {@link ShenyuSdkClientFactory}.
 */
public class ShenyuSdkClientFactoryTest {

    @BeforeEach
    public void setUp() throws Exception {
        final Field field = ShenyuSdkClientFactory.class.getDeclaredField(
                "SDK_CLIENT_MAP");
        field.setAccessible(true);
        final Map<String, ShenyuSdkClient> map = (Map<String, ShenyuSdkClient>) field.get(
                null);
        map.put("httpclient", mock(ShenyuSdkClient.class));
    }

    @Test
    public void testNewInstance() {
        assertNotNull(ShenyuSdkClientFactory.newInstance("httpclient"));
        try (MockedStatic<ExtensionLoader> mocked = mockStatic(ExtensionLoader.class)) {
            ExtensionLoader extensionLoader = mock(ExtensionLoader.class);
            mocked.when(() -> ExtensionLoader.getExtensionLoader(ShenyuSdkClient.class))
                    .thenReturn(extensionLoader);
            when(extensionLoader.getJoin("clientType")).thenReturn(
                    mock(ShenyuSdkClient.class));
            assertNotNull(ShenyuSdkClientFactory.newInstance("clientType"));
        }
    }
}
