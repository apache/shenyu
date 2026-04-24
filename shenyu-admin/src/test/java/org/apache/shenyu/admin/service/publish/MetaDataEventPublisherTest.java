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

package org.apache.shenyu.admin.service.publish;

import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.event.metadata.MetaDataCreatedEvent;
import org.apache.shenyu.admin.model.event.metadata.MetadataUpdatedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link MetaDataEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class MetaDataEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private MetaDataEventPublisher metaDataEventPublisher;

    @BeforeEach
    void setUp() {
        metaDataEventPublisher = new MetaDataEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreated() {
        MetaDataDO metaData = buildMetaDataDO("1", "test-path", "test-service");

        metaDataEventPublisher.onCreated(metaData);

        ArgumentCaptor<MetaDataCreatedEvent> captor = ArgumentCaptor.forClass(MetaDataCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        MetaDataCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(metaData, event.getMetaData());
    }

    @Test
    void testOnUpdated() {
        MetaDataDO before = buildMetaDataDO("1", "old-path", "test-service");
        MetaDataDO after = buildMetaDataDO("1", "new-path", "test-service");

        metaDataEventPublisher.onUpdated(after, before);

        ArgumentCaptor<MetadataUpdatedEvent> captor = ArgumentCaptor.forClass(MetadataUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        MetadataUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @ParameterizedTest
    @MethodSource("provideMetaDataListsForDeleted")
    void testOnDeleted(final List<MetaDataDO> metaDataList) {
        metaDataEventPublisher.onDeleted(metaDataList);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    private static Stream<List<MetaDataDO>> provideMetaDataListsForDeleted() {
        MetaDataDO metaData1 = buildMetaDataDO("1", "test-path", "test-service");
        MetaDataDO metaData2 = buildMetaDataDO("1", "path1", "service1");
        MetaDataDO metaData3 = buildMetaDataDO("2", "path2", "service2");
        MetaDataDO metaData4 = buildMetaDataDO("3", "path3", "service3");

        return Stream.of(
            Collections.singletonList(metaData1),
            Arrays.asList(metaData2, metaData3, metaData4),
            Collections.emptyList()
        );
    }

    @ParameterizedTest
    @MethodSource("provideMetaDataListsForEnabled")
    void testOnEnabled(final List<MetaDataDO> metaDataList) {
        metaDataEventPublisher.onEnabled(metaDataList);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    private static Stream<List<MetaDataDO>> provideMetaDataListsForEnabled() {
        MetaDataDO metaData1 = buildMetaDataDO("1", "test-path", "test-service");
        MetaDataDO metaData2 = buildMetaDataDO("1", "path1", "service1");
        MetaDataDO metaData3 = buildMetaDataDO("2", "path2", "service2");

        return Stream.of(
            Collections.singletonList(metaData1),
            Arrays.asList(metaData2, metaData3),
            Collections.emptyList()
        );
    }

    @Test
    void testPublish() {
        MetaDataDO metaData = buildMetaDataDO("1", "test-path", "test-service");
        MetaDataCreatedEvent event = new MetaDataCreatedEvent(metaData, "test-user");

        metaDataEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    @Test
    void testMultipleOperations() {
        MetaDataDO metaData1 = buildMetaDataDO("1", "path1", "service1");
        MetaDataDO metaData2 = buildMetaDataDO("2", "path2", "service2");
        MetaDataDO metaData3 = buildMetaDataDO("3", "path3", "service3");

        metaDataEventPublisher.onCreated(metaData1);
        metaDataEventPublisher.onUpdated(metaData2, metaData1);
        metaDataEventPublisher.onDeleted(Collections.singletonList(metaData3));
        metaDataEventPublisher.onEnabled(Collections.singletonList(metaData1));

        verify(applicationEventPublisher, times(6)).publishEvent(any());
    }

    private static MetaDataDO buildMetaDataDO(final String id, final String path, final String serviceName) {
        MetaDataDO metaData = new MetaDataDO();
        metaData.setId(id);
        metaData.setPath(path);
        metaData.setServiceName(serviceName);
        metaData.setMethodName("testMethod");
        metaData.setRpcType("http");
        metaData.setEnabled(true);
        return metaData;
    }
}
