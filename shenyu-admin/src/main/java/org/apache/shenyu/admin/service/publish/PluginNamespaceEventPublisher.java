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
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginNamespaceChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginNamespaceChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginNamespaceCreatedEvent;
import org.apache.shenyu.admin.model.vo.PluginNamespaceVO;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * PluginEventPublisher.
 */
@Component
public class PluginNamespaceEventPublisher implements AdminDataModelChangedEventPublisher<PluginNamespaceVO> {

    private final ApplicationEventPublisher publisher;

    public PluginNamespaceEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }


    /**
     * on plugin created.
     *
     * @param pluginNamespaceVO pluginNamespaceVO
     */
    @Override
    public void onCreated(PluginNamespaceVO pluginNamespaceVO) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.CREATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(pluginNamespaceVO))));
        publish(new PluginNamespaceCreatedEvent(pluginNamespaceVO, SessionUtil.visitorName()));
    }

    @Override
    public void onUpdated(PluginNamespaceVO pluginNamespaceVO, PluginNamespaceVO before) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(pluginNamespaceVO))));
        publish(new PluginNamespaceChangedEvent(pluginNamespaceVO, before, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
    }

    @Override
    public void onDeleted(PluginNamespaceVO pluginNamespaceVO) {
        publish(new PluginNamespaceChangedEvent(pluginNamespaceVO, null, EventTypeEnum.PLUGIN_DELETE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                Stream.of(pluginNamespaceVO).map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }

    @Override
    public void onDeleted(Collection<PluginNamespaceVO> pluginNamespaces) {
        publish(new BatchPluginNamespaceChangedEvent(pluginNamespaces, null, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                pluginNamespaces.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }


    /**
     * on plugin namespace batch enabled.
     *
     * @param pluginNamespaces pluginNamespaces
     */
    public void onEnabled(final Collection<PluginNamespaceVO> pluginNamespaces) {
        publish(new BatchPluginNamespaceChangedEvent(pluginNamespaces, null, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                pluginNamespaces.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }

    @Override
    public void publish(AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }
}
