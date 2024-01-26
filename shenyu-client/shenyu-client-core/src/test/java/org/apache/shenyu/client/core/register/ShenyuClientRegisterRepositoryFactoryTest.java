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
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory.getRepositoryMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

@ExtendWith(MockitoExtension.class)
public class ShenyuClientRegisterRepositoryFactoryTest {

    @Mock
    private ShenyuClientRegisterRepository repositoryMock;

    @Mock
    private ShenyuRegisterCenterConfig configMock = new ShenyuRegisterCenterConfig();

    @InjectMocks
    private ShenyuClientRegisterRepositoryFactory factory;

    @Test
    public void testNewInstanceRegisterNewRepository() {
        // Mock ExtensionLoader.getJoin() to return our mock repository
        when(ExtensionLoader.getExtensionLoader(ShenyuClientRegisterRepository.class).getJoin("someType is error"))
                .thenReturn(repositoryMock);

        // Configure config mock
        when(configMock.getRegisterType()).thenReturn("someType");

        // Call the method and verify if expected repository is returned
        ShenyuClientRegisterRepository actualRepository = ShenyuClientRegisterRepositoryFactory.newInstance(configMock);
        assertEquals(repositoryMock, actualRepository);

        // Verify if init and put were called on repository and map
        verify(repositoryMock).init(configMock);
        verify(getRepositoryMap()).put("someType", repositoryMock);
    }

    @Test
    public void testNewInstanceExistingRepository() {
        // Configure REPOSITORY_MAP with a pre-existing repository
        ShenyuClientRegisterRepository preExistingRepository = Mockito.mock(ShenyuClientRegisterRepository.class);

        Map<String, ShenyuClientRegisterRepository> testRepositoryMap = new ConcurrentHashMap<>();
        testRepositoryMap.put("existingType", preExistingRepository);
        ShenyuClientRegisterRepositoryFactory.setRepositoryMap(testRepositoryMap);

        // Call the method and verify that the existing repository is returned
        ShenyuClientRegisterRepository actualRepository = ShenyuClientRegisterRepositoryFactory.newInstance(configMock);
        assertEquals(preExistingRepository, actualRepository);

        // Verify that no new repository was created or initialized
        verify(testRepositoryMap, never()).put(anyString(), any(ShenyuClientRegisterRepository.class));
        verifyNoMoreInteractions(repositoryMock);
    }
}
