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
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

// /**
//  * Test for {@link ShenyuClientMetadataExecutorSubscriber}.
//  */
@ExtendWith(MockitoExtension.class)
class ShenyuClientMetadataExecutorSubscriberTest {

    @Mock
    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    @InjectMocks
    private ShenyuClientMetadataExecutorSubscriber subscriber;

    @Test
    public void getType_ShouldReturnMetaDataType() {
        DataType result = subscriber.getType();
        assertEquals(DataType.META_DATA, result, "getType should return META_DATA");
    }

    @Test
    public void executor_ShouldPersistMetaDataRegisterDTOList() {
        // Arrange
        MetaDataRegisterDTO metaDataRegisterDTO1 = new MetaDataRegisterDTOl
        MetaDataRegisterDTO metaDataRegisterDTO2 = new MetaDataRegisterDTO;
        Collection<MetaDataRegisterDTO> metaDataRegisterDTOList = Arrays.asList(metaDataRegisterDTO1,
                metaDataRegisterDTO2);

       
        subscriber.executor(metaDataRegisterDTOList);

  
        verify(shenyuClientRegisterRepository, times(2)).persistInterface(any());

    }

    @Test
    void executor_ShouldNotPersistIfMetaDataRegisterDTOListIsEmpty() {
       
        Collection<MetaDataRegisterDTO> emptyList = Arrays.asList();

      
        subscriber.executor(emptyList);

     
        verify(shenyuClientRegisterRepository, times(0)).persistInterface(any());
        
    }

    @Test
    void executor_ShouldHandleSingleMetaDataRegisterDTO() {
        
        MetaDataRegisterDTO metaDataRegisterDTO = new MetaDataRegisterDTO;
        Collection<MetaDataRegisterDTO> metaDataRegisterDTOList = Collections.singletonList(metaDataRegisterDTO);

      
        subscriber.executor(metaDataRegisterDTOList);

     
        verify(shenyuClientRegisterRepository, times(1)).persistInterface(metaDataRegisterDTO);
    }

}
