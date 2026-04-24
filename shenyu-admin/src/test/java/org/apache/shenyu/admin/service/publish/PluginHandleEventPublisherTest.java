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

import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.event.handle.BatchPluginHandleChangedEvent;
import org.apache.shenyu.admin.model.event.handle.PluginHandleChangedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link PluginHandleEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class PluginHandleEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private PluginHandleEventPublisher pluginHandleEventPublisher;

    @BeforeEach
    void setUp() {
        pluginHandleEventPublisher = new PluginHandleEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreated() {
        PluginHandleDO pluginHandle = buildPluginHandleDO("1", "test-field");

        pluginHandleEventPublisher.onCreated(pluginHandle);

        ArgumentCaptor<PluginHandleChangedEvent> captor = ArgumentCaptor.forClass(PluginHandleChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        PluginHandleChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(pluginHandle, event.getAfter());
    }

    @Test
    void testOnUpdated() {
        PluginHandleDO before = buildPluginHandleDO("1", "old-field");
        PluginHandleDO after = buildPluginHandleDO("1", "new-field");

        pluginHandleEventPublisher.onUpdated(after, before);

        ArgumentCaptor<PluginHandleChangedEvent> captor = ArgumentCaptor.forClass(PluginHandleChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        PluginHandleChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeletedSingle() {
        PluginHandleDO pluginHandle = buildPluginHandleDO("1", "test-field");

        pluginHandleEventPublisher.onDeleted(pluginHandle);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testOnDeletedCollection() {
        PluginHandleDO handle1 = buildPluginHandleDO("1", "field1");
        PluginHandleDO handle2 = buildPluginHandleDO("2", "field2");
        List<PluginHandleDO> handles = Arrays.asList(handle1, handle2);

        pluginHandleEventPublisher.onDeleted(handles);

        ArgumentCaptor<BatchPluginHandleChangedEvent> captor = ArgumentCaptor.forClass(BatchPluginHandleChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchPluginHandleChangedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<PluginHandleDO> emptyList = Collections.emptyList();

        pluginHandleEventPublisher.onDeleted(emptyList);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testPublish() {
        PluginHandleDO pluginHandle = buildPluginHandleDO("1", "test-field");
        PluginHandleChangedEvent event = new PluginHandleChangedEvent(pluginHandle, null, null, "test-user");

        pluginHandleEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static PluginHandleDO buildPluginHandleDO(final String id, final String field) {
        PluginHandleDO pluginHandle = new PluginHandleDO();
        pluginHandle.setId(id);
        pluginHandle.setField(field);
        pluginHandle.setPluginId("test-plugin-id");
        pluginHandle.setLabel("test-label");
        return pluginHandle;
    }
}
