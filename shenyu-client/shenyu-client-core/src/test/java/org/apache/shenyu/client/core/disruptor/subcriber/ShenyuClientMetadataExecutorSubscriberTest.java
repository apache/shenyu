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

package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.times;

/**
 * Test for {@link ShenyuClientMetadataExecutorSubscriber}.
 */
public class ShenyuClientMetadataExecutorSubscriberTest {

    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    private ShenyuClientMetadataExecutorSubscriber executorSubscriber;

    @BeforeEach
    public void setUp() {
        shenyuClientRegisterRepository = mock(ShenyuClientRegisterRepository.class);
        executorSubscriber = new ShenyuClientMetadataExecutorSubscriber(shenyuClientRegisterRepository);
    }

    @Test
    public void testGetType() {
        DataType expected = DataType.META_DATA;
        DataType actual = executorSubscriber.getType();
        assertEquals(expected, actual);
    }

    @Test
    public void testExecutorWithEmptyData() {
        Collection<MetaDataRegisterDTO> metaDataRegisterDTOList = new ArrayList<>();
        executorSubscriber.executor(metaDataRegisterDTOList);

        verify(shenyuClientRegisterRepository, never()).persistInterface(any());
    }

    @Test
    public void testExecutorValidData() {
        Collection<MetaDataRegisterDTO> metaList = new ArrayList<>();

        MetaDataRegisterDTO metaDataRegisterDTO =
                MetaDataRegisterDTO.builder().contextPath("/test").path("/meta").pathDesc("application/json")
                        .rpcType("http")
                        .serviceName("UserService")
                        .methodName("getUserInfo")
                        .ruleName("AuthorizationRule")
                        .parameterTypes("String, int")
                        .rpcExt("test")
                        .enabled(true)
                        .host("localhost")
                        .port(8080)
                        .pluginNames(new ArrayList<>())
                        .build();
        metaList.add(metaDataRegisterDTO);

        executorSubscriber.executor(metaList);

        verify(shenyuClientRegisterRepository, times(1)).persistInterface(metaDataRegisterDTO);
    }

}
