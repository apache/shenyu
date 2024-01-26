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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory.getRepositoryMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;

public class ShenyuClientRegisterRepositoryFactoryTest {
    @Mock
    private ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();

    @Mock
    private ShenyuClientRegisterRepository repositoryMock;

    @Mock
    private ExtensionLoader<DataTypeParent> extensionLoader;

    @BeforeEach
    public void setUp() {
        // Reset REPOSITORY_MAP before each test to ensure isolation
        ShenyuClientRegisterRepositoryFactory.getRepositoryMap().clear();
    }

    @Test
    public void testNewInstanceRegisterNewRepository() {
        when(config.getRegisterType()).thenReturn("someType");
        when(extensionLoader.getJoin(anyString())).thenReturn((DataTypeParent) repositoryMock);

        // Exercise the method under test
        ShenyuClientRegisterRepository actualRepository = ShenyuClientRegisterRepositoryFactory.newInstance(config);

        // Assertions
        Assertions.assertEquals(repositoryMock, actualRepository);
        verify(repositoryMock).init(config);
        verify(getRepositoryMap()).put("someType", repositoryMock);
    }

    @Test
    public void testNewInstanceReturnsExistingRepository() {
        // Pre-populate the REPOSITORY_MAP
        ShenyuClientRegisterRepositoryFactory.getRepositoryMap().put("someType", repositoryMock);

        when(config.getRegisterType()).thenReturn("someType");

        // Exercise the method under test
        ShenyuClientRegisterRepository actualRepository = ShenyuClientRegisterRepositoryFactory.newInstance(config);

        // Assertions
        Assertions.assertSame(repositoryMock, actualRepository);
        verify(repositoryMock, never()).init(config);
        verify(getRepositoryMap(), never()).put("someType", repositoryMock);
    }
}
