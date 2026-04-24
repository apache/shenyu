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

import org.apache.shenyu.admin.model.event.plugin.BatchNamespacePluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchNamespacePluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.NamespacePluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.NamespacePluginCreatedEvent;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
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
 * Test case for {@link NamespacePluginEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class NamespacePluginEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private NamespacePluginEventPublisher namespacePluginEventPublisher;

    @BeforeEach
    void setUp() {
        namespacePluginEventPublisher = new NamespacePluginEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreated() {
        NamespacePluginVO plugin = buildNamespacePluginVO("1", "test-plugin", "test-namespace");

        namespacePluginEventPublisher.onCreated(plugin);

        ArgumentCaptor<NamespacePluginCreatedEvent> captor = ArgumentCaptor.forClass(NamespacePluginCreatedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        NamespacePluginCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(plugin, event.getPlugin());
    }

    @Test
    void testOnUpdated() {
        NamespacePluginVO before = buildNamespacePluginVO("1", "old-plugin", "test-namespace");
        NamespacePluginVO after = buildNamespacePluginVO("1", "new-plugin", "test-namespace");

        namespacePluginEventPublisher.onUpdated(after, before);

        ArgumentCaptor<NamespacePluginChangedEvent> captor = ArgumentCaptor.forClass(NamespacePluginChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        NamespacePluginChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeletedSingle() {
        NamespacePluginVO plugin = buildNamespacePluginVO("1", "test-plugin", "test-namespace");

        namespacePluginEventPublisher.onDeleted(plugin);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedCollection() {
        NamespacePluginVO plugin1 = buildNamespacePluginVO("1", "plugin1", "test-namespace");
        NamespacePluginVO plugin2 = buildNamespacePluginVO("2", "plugin2", "test-namespace");
        List<NamespacePluginVO> plugins = Arrays.asList(plugin1, plugin2);

        namespacePluginEventPublisher.onDeleted(plugins);

        ArgumentCaptor<BatchNamespacePluginDeletedEvent> captor = ArgumentCaptor.forClass(BatchNamespacePluginDeletedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchNamespacePluginDeletedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(2, event.getDeletedPluginIds().size());
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<NamespacePluginVO> emptyList = Collections.emptyList();

        namespacePluginEventPublisher.onDeleted(emptyList);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnEnabled() {
        NamespacePluginVO plugin1 = buildNamespacePluginVO("1", "plugin1", "test-namespace");
        NamespacePluginVO plugin2 = buildNamespacePluginVO("2", "plugin2", "test-namespace");
        List<NamespacePluginVO> plugins = Arrays.asList(plugin1, plugin2);

        namespacePluginEventPublisher.onEnabled(plugins);

        ArgumentCaptor<BatchNamespacePluginChangedEvent> captor = ArgumentCaptor.forClass(BatchNamespacePluginChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchNamespacePluginChangedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testPublish() {
        NamespacePluginVO plugin = buildNamespacePluginVO("1", "test-plugin", "test-namespace");
        NamespacePluginCreatedEvent event = new NamespacePluginCreatedEvent(plugin, "test-user");

        namespacePluginEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static NamespacePluginVO buildNamespacePluginVO(final String id, final String name, final String namespaceId) {
        NamespacePluginVO plugin = new NamespacePluginVO();
        plugin.setId(id);
        plugin.setName(name);
        plugin.setNamespaceId(namespaceId);
        plugin.setEnabled(true);
        plugin.setRole("test-role");
        plugin.setConfig("test-config");
        return plugin;
    }
}
