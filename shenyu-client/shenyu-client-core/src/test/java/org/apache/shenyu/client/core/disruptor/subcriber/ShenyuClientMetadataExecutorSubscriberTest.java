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
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * Test for {@link ShenyuClientMetadataExecutorSubscriber}.
 */
@ExtendWith(MockitoExtension.class)
class ShenyuClientMetadataExecutorSubscriberTest {

    @Mock
    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    @InjectMocks
    private ShenyuClientMetadataExecutorSubscriber subscriber;

    @Test
    void shouldReturnMetaDataDataType() {
        assertEquals(DataType.META_DATA, subscriber.getType());
    }

    @Test
    void shouldPersistInterfaceData() {
        // Arrange
        MetaDataRegisterDTO metaDataRegisterDTO = new MetaDataRegisterDTO();
        Collection<MetaDataRegisterDTO> metaDataList = Arrays.asList(metaDataRegisterDTO);

        // Act
        subscriber.executor(metaDataList);

        // Assert
        Mockito.verify(shenyuClientRegisterRepository, Mockito.times(1)).persistInterface(metaDataRegisterDTO);
    }

    @Test
    void shouldNotPersistIfMetaDataListIsEmpty() {
        // Arrange
        Collection<MetaDataRegisterDTO> emptyMetaDataList = Collections.emptyList();

        // Act
        subscriber.executor(emptyMetaDataList);

        // Assert
        Mockito.verify(shenyuClientRegisterRepository, Mockito.never()).persistInterface(Mockito.any());
    }

    @Test
    void shouldHandleExceptionDuringPersistence() {
        // Arrange
        MetaDataRegisterDTO metaDataRegisterDTO = new MetaDataRegisterDTO();
        Collection<MetaDataRegisterDTO> metaDataList = Arrays.asList(metaDataRegisterDTO);

        // Mocking an exception during persistence
        Mockito.doThrow(new RuntimeException("Simulating persistence failure")).when(shenyuClientRegisterRepository)
                .persistInterface(metaDataRegisterDTO);

        // Act & Assert
        assertThrows(RuntimeException.class, () -> subscriber.executor(metaDataList));
    }

}
