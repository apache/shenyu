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
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchPluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;

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
    }

    /**
     * on plugin deleted.
     *
     * @param plugins plugins
     */
    @Override
    public void onDeleted(final Collection<PluginDO> plugins) {
        publish(new BatchPluginDeletedEvent(plugins, SessionUtil.visitorName()));
    }

    /**
     * on plugin batch enabled.
     *
     * @param plugins plugins
     */
    public void onEnabled(final Collection<PluginDO> plugins) {
        publish(new BatchPluginChangedEvent(plugins, null, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
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
