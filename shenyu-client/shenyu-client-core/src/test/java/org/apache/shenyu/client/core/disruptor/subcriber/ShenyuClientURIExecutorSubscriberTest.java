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

import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test for {@link ShenyuClientURIExecutorSubscriber}.
 */
public class ShenyuClientURIExecutorSubscriberTest {

    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    private ShenyuClientURIExecutorSubscriber executorSubscriber;

    @BeforeEach
    public void setUp() {
        shenyuClientRegisterRepository = mock(ShenyuClientRegisterRepository.class);
        executorSubscriber = new ShenyuClientURIExecutorSubscriber(shenyuClientRegisterRepository);

        // set properties to avoid NullPointerException
        Properties properties = new Properties();
        ShenyuClientShutdownHook.set(shenyuClientRegisterRepository, properties);
    }

    @Test
    public void testGetType() {
        DataType expected = DataType.URI;
        DataType actual = executorSubscriber.getType();
        assertEquals(expected, actual);
    }

    @Test
    public void testExecutorWithEmptyData() {
        Collection<URIRegisterDTO> dataList = new ArrayList<>();
        executorSubscriber.executor(dataList);

        verify(shenyuClientRegisterRepository, never()).persistApiDoc(any());
    }

    @Test
    public void testExecutorValidData() throws IOException {

        // open port for connection
        ServerSocket socket = new ServerSocket(9527);

        Collection<URIRegisterDTO> uriRegisterDTOList = new ArrayList<>();

        URIRegisterDTO uriRegisterDTO =
                URIRegisterDTO.builder().protocol("http").contextPath("/test").rpcType("http").host("localhost").eventType(EventType.REGISTER).port(9527).build();
        uriRegisterDTOList.add(uriRegisterDTO);

        executorSubscriber.executor(uriRegisterDTOList);
        verify(shenyuClientRegisterRepository, times(1)).persistURI(uriRegisterDTO);
    }
}
