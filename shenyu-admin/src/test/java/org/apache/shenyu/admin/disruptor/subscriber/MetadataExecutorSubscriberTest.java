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
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
    
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
    
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
    
/**
 * Test cases for {@link MetadataExecutorSubscriber}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class MetadataExecutorSubscriberTest {

    @InjectMocks
    private MetadataExecutorSubscriber metadataExecutorSubscriber;
    
    @Mock
    private Map<String, ShenyuClientRegisterService> shenyuClientRegisterService;
    
    @Test
    public void testGetType() {
        assertEquals(DataType.META_DATA, metadataExecutorSubscriber.getType());
    }
    
    @Test
    public void testExecutor() {
        List<MetaDataRegisterDTO> list = new ArrayList<>();
        metadataExecutorSubscriber.executor(list);
        assertEquals(true, list.isEmpty());
        list.add(MetaDataRegisterDTO.builder().appName("test").build());
        ShenyuClientRegisterService service = mock(ShenyuClientRegisterService.class);
        when(shenyuClientRegisterService.get(any())).thenReturn(service);
        metadataExecutorSubscriber.executor(list);
        verify(service).register(any());
    }
}
