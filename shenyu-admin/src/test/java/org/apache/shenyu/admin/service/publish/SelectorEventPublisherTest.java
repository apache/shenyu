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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.selector.SelectorChangedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorCreatedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorUpdatedEvent;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link SelectorEventPublisher}.
 */
@ExtendWith(MockitoExtension.class)
class SelectorEventPublisherTest {

    private static final String TEST_OPERATOR = "test-operator";

    private static final String TEST_PLUGIN_NAME = "test-plugin";

    private static final String DIVIDE_PLUGIN_NAME = "divide";

    @Mock
    private ApplicationEventPublisher applicationEventPublisher;

    private SelectorEventPublisher selectorEventPublisher;

    private MockedStatic<SessionUtil> sessionUtilMockedStatic;

    private MockedStatic<UpstreamCheckService> upstreamCheckServiceMockedStatic;

    @BeforeEach
    void setUp() {
        selectorEventPublisher = new SelectorEventPublisher(applicationEventPublisher);
        sessionUtilMockedStatic = mockStatic(SessionUtil.class);
        sessionUtilMockedStatic.when(SessionUtil::visitorName).thenReturn(TEST_OPERATOR);
        upstreamCheckServiceMockedStatic = mockStatic(UpstreamCheckService.class);
    }

    @AfterEach
    void tearDown() {
        sessionUtilMockedStatic.close();
        upstreamCheckServiceMockedStatic.close();
    }

    @Test
    void testOnCreated() {
        SelectorDO selector = buildSelectorDO("1", "plugin1", "test-selector");

        selectorEventPublisher.onCreated(selector);

        ArgumentCaptor<SelectorCreatedEvent> captor = ArgumentCaptor.forClass(SelectorCreatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        SelectorCreatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(selector, event.getSelector());
    }

    @Test
    void testOnUpdated() {
        SelectorDO before = buildSelectorDO("1", "plugin1", "old-selector");
        SelectorDO after = buildSelectorDO("1", "plugin1", "new-selector");

        selectorEventPublisher.onUpdated(after, before);

        ArgumentCaptor<SelectorUpdatedEvent> captor = ArgumentCaptor.forClass(SelectorUpdatedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        SelectorUpdatedEvent event = captor.getValue();
        assertNotNull(event);
        assertEquals(after, event.getAfter());
        assertEquals(before, event.getBefore());
    }

    @Test
    void testOnDeletedSingle() {
        SelectorDO selector = buildSelectorDO("1", "plugin1", "test-selector");

        selectorEventPublisher.onDeleted(selector);

        ArgumentCaptor<SelectorChangedEvent> captor = ArgumentCaptor.forClass(SelectorChangedEvent.class);
        verify(applicationEventPublisher, times(1)).publishEvent(captor.capture());

        SelectorChangedEvent event = captor.getValue();
        assertNotNull(event);
    }

    @Test
    void testOnDeletedCollection() {
        SelectorDO selector1 = buildSelectorDO("1", "plugin1", "selector1");
        SelectorDO selector2 = buildSelectorDO("2", "plugin1", "selector2");
        List<SelectorDO> selectors = Arrays.asList(selector1, selector2);

        PluginDO plugin = buildPluginDO("plugin1", TEST_PLUGIN_NAME);
        List<PluginDO> plugins = Collections.singletonList(plugin);

        selectorEventPublisher.onDeleted(selectors, plugins);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedCollectionWithDividePlugin() {
        SelectorDO selector1 = buildSelectorDO("1", "plugin1", "selector1");
        SelectorDO selector2 = buildSelectorDO("2", "plugin1", "selector2");
        List<SelectorDO> selectors = Arrays.asList(selector1, selector2);

        PluginDO plugin = buildPluginDO("plugin1", DIVIDE_PLUGIN_NAME);
        List<PluginDO> plugins = Collections.singletonList(plugin);

        selectorEventPublisher.onDeleted(selectors, plugins);

        verify(applicationEventPublisher, times(2)).publishEvent(any());

        upstreamCheckServiceMockedStatic.verify(() -> UpstreamCheckService.removeByKey("1"), times(1));
        upstreamCheckServiceMockedStatic.verify(() -> UpstreamCheckService.removeByKey("2"), times(1));
    }

    @Test
    void testOnDeletedEmptyCollection() {
        List<SelectorDO> emptySelectors = Collections.emptyList();
        List<PluginDO> emptyPlugins = Collections.emptyList();

        selectorEventPublisher.onDeleted(emptySelectors, emptyPlugins);

        verify(applicationEventPublisher, times(2)).publishEvent(any());
    }

    @Test
    void testOnDeletedCollectionWithMultiplePlugins() {
        SelectorDO selector1 = buildSelectorDO("1", "plugin1", "selector1");
        SelectorDO selector2 = buildSelectorDO("2", "plugin2", "selector2");
        List<SelectorDO> selectors = Arrays.asList(selector1, selector2);

        PluginDO plugin1 = buildPluginDO("plugin1", TEST_PLUGIN_NAME);
        PluginDO plugin2 = buildPluginDO("plugin2", "another-plugin");
        List<PluginDO> plugins = Arrays.asList(plugin1, plugin2);

        selectorEventPublisher.onDeleted(selectors, plugins);

        ArgumentCaptor<DataChangedEvent> dataChangedCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(applicationEventPublisher, times(2)).publishEvent(any());
        verify(applicationEventPublisher).publishEvent(dataChangedCaptor.capture());

        DataChangedEvent event = dataChangedCaptor.getValue();
        assertNotNull(event);
        assertEquals(ConfigGroupEnum.SELECTOR, event.getGroupKey());
    }

    @Test
    void testPublish() {
        SelectorDO selector = buildSelectorDO("1", "plugin1", "test-selector");
        SelectorCreatedEvent event = new SelectorCreatedEvent(selector, TEST_OPERATOR);

        selectorEventPublisher.publish(event);

        verify(applicationEventPublisher, times(1)).publishEvent(event);
    }

    private static SelectorDO buildSelectorDO(final String id, final String pluginId, final String selectorName) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        SelectorDO selectorDO = SelectorDO.builder()
                .id(id)
                .pluginId(pluginId)
                .selectorName(selectorName)
                .matchMode(0)
                .enabled(true)
                .loged(false)
                .sortCode(1)
                .handle("{}")
                .matchRestful(false)
                .build();
        selectorDO.setDateCreated(currentTime);
        selectorDO.setDateUpdated(currentTime);
        return selectorDO;
    }

    private static PluginDO buildPluginDO(final String id, final String name) {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId(id);
        pluginDO.setName(name);
        return pluginDO;
    }
}
