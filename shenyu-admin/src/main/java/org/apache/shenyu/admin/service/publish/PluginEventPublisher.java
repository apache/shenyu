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
import org.apache.shenyu.admin.model.event.plugin.PluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
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
public class PluginEventPublisher implements AdminDataModelChangedEventPublisher<PluginDO> {
    
    private final ApplicationEventPublisher publisher;
    
    public PluginEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }
    
    /**
     * on plugin created.
     *
     * @param plugin plugin
     */
    @Override
    public void onCreated(final PluginDO plugin) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.CREATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(plugin))));
        publish(new PluginCreatedEvent(plugin, SessionUtil.visitorName()));
    }
    
    /**
     * on plugin updated.
     *
     * @param plugin plugin
     * @param before before plugin
     */
    @Override
    public void onUpdated(final PluginDO plugin, final PluginDO before) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(plugin))));
        publish(new PluginChangedEvent(plugin, before, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
    }
    
    /**
     * on plugin deleted.
     *
     * @param plugin plugin
     */
    @Override
    public void onDeleted(final PluginDO plugin) {
        publish(new PluginChangedEvent(plugin, null, EventTypeEnum.PLUGIN_DELETE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                Stream.of(plugin).map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }
    
    /**
     * on plugin deleted.
     *
     * @param plugins plugins
     */
    @Override
    public void onDeleted(final Collection<PluginDO> plugins) {
        publish(new BatchPluginDeletedEvent(plugins, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                plugins.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }
    
    /**
     * on plugin batch enabled.
     *
     * @param plugins plugins
     */
    public void onEnabled(final Collection<PluginDO> plugins) {
        publish(new BatchPluginChangedEvent(plugins, null, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                plugins.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }
    
    /**
     * event.
     *
     * @param event event.
     */
    @Override
    public void publish(final AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }
}
