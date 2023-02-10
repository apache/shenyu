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
    
package org.apache.shenyu.admin.disruptor.subscriber;
    
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
    
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
    
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
    
/**
 * Test cases for {@link URIRegisterExecutorSubscriber}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class URIRegisterExecutorSubscriberTest {
    
    @InjectMocks
    private URIRegisterExecutorSubscriber uriRegisterExecutorSubscriber;
    
    @Mock
    private Map<String, ShenyuClientRegisterService> shenyuClientRegisterService;
    
    @Test
    public void testGetType() {
        assertEquals(DataType.URI, uriRegisterExecutorSubscriber.getType());
    }
    
    @Test
    public void testExecutor() {
        List<URIRegisterDTO> list = new ArrayList<>();
        uriRegisterExecutorSubscriber.executor(list);
        assertEquals(true, list.isEmpty());
        list.add(URIRegisterDTO.builder().rpcType(RpcTypeEnum.HTTP.getName())
                .appName("test").contextPath("/test").build());
        ShenyuClientRegisterService service = mock(ShenyuClientRegisterService.class);
        when(shenyuClientRegisterService.get(any())).thenReturn(service);
        uriRegisterExecutorSubscriber.executor(list);
        verify(service).registerURI(any(), any());
    }
    
    @Test
    public void testBuildData() {
        try {
            List<URIRegisterDTO> list = new ArrayList<>();
            list.add(URIRegisterDTO.builder().appName("test1").build());
            list.add(URIRegisterDTO.builder().appName("test2").build());
            Method testMethod = uriRegisterExecutorSubscriber.getClass().getDeclaredMethod("buildData", Collection.class);
            testMethod.setAccessible(true);
            Map<String, List<URIRegisterDTO>> result = (Map) testMethod.invoke(uriRegisterExecutorSubscriber, list);
            assertEquals(2, result.size());
            
            list.add(URIRegisterDTO.builder().appName("test1").build());
            result = (Map) testMethod.invoke(uriRegisterExecutorSubscriber, list);
            assertEquals(2, result.size());
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }
}
