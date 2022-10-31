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

package org.apache.shenyu.register.instance.core;

import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
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

public final class ShenyuInstanceRegisterRepositoryFactoryTest {

    @BeforeEach
    public void setup() throws Exception {
        final Field field = ShenyuInstanceRegisterRepositoryFactory.class.getDeclaredField("REPOSITORY_MAP");
        field.setAccessible(true);
        final Map map = (Map) field.get(null);
        map.put("zookeeper", mock(ShenyuInstanceRegisterRepository.class));
    }

    @Test
    public void testNewInstance() {
        assertNotNull(ShenyuInstanceRegisterRepositoryFactory.newInstance("zookeeper"));
        try (MockedStatic<ExtensionLoader> extensionLoaderMockedStatic = mockStatic(ExtensionLoader.class)) {
            ExtensionLoader extensionLoader = mock(ExtensionLoader.class);
            extensionLoaderMockedStatic.when(() -> ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class)).thenReturn(extensionLoader);
            when(extensionLoader.getJoin("zs")).thenReturn(mock(ShenyuInstanceRegisterRepository.class));
            assertNotNull(ShenyuInstanceRegisterRepositoryFactory.newInstance("zs"));
        }
    }
}
