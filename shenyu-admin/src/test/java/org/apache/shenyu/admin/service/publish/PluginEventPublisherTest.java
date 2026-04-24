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

import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
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
 * Test case for {@link PluginEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class PluginEventPublisherTest {

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private PluginEventPublisher pluginEventPublisher;

    @BeforeEach
    void setUp() {
        pluginEventPublisher = new PluginEventPublisher(applicationEventPublisher);
    }

    @Test
    void testOnCreated() {
        PluginDO plugin = buildPluginDO("1", "test-plugin");

        pluginEventPublisher.onCreated(plugin);

        ArgumentCaptor<PluginCreatedEvent> captor = ArgumentCaptor.forClass(PluginCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        PluginCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(plugin, event.getPlugin());
    }

    @Test
    void testOnUpdated() {
        PluginDO before = buildPluginDO("1", "old-plugin");
        PluginDO after = buildPluginDO("1", "new-plugin");

        pluginEventPublisher.onUpdated(after, before);

        ArgumentCaptor<PluginChangedEvent> captor = ArgumentCaptor.forClass(PluginChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        PluginChangedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeletedSingle() {
        PluginDO plugin = buildPluginDO("1", "test-plugin");

        pluginEventPublisher.onDeleted(plugin);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testOnDeletedCollection() {
        PluginDO plugin1 = buildPluginDO("1", "plugin1");
        PluginDO plugin2 = buildPluginDO("2", "plugin2");
        List<PluginDO> plugins = Arrays.asList(plugin1, plugin2);

        pluginEventPublisher.onDeleted(plugins);

        ArgumentCaptor<BatchPluginDeletedEvent> captor = ArgumentCaptor.forClass(BatchPluginDeletedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchPluginDeletedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(2, event.getDeletedPluginIds().size());
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<PluginDO> emptyList = Collections.emptyList();

        pluginEventPublisher.onDeleted(emptyList);

        verify(applicationEventPublisher, times(1)).publishEvent(any());
    }

    @Test
    void testOnEnabled() {
        PluginDO plugin1 = buildPluginDO("1", "plugin1");
        PluginDO plugin2 = buildPluginDO("2", "plugin2");
        List<PluginDO> plugins = Arrays.asList(plugin1, plugin2);

        pluginEventPublisher.onEnabled(plugins);

        ArgumentCaptor<BatchPluginChangedEvent> captor = ArgumentCaptor.forClass(BatchPluginChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        BatchPluginChangedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testPublish() {
        PluginDO plugin = buildPluginDO("1", "test-plugin");
        PluginCreatedEvent event = new PluginCreatedEvent(plugin, "test-user");

        pluginEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static PluginDO buildPluginDO(final String id, final String name) {
        PluginDO plugin = new PluginDO();
        plugin.setId(id);
        plugin.setName(name);
        plugin.setEnabled(true);
        plugin.setRole("test-role");
        plugin.setConfig("test-config");
        return plugin;
    }
}
