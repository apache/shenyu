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
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.apache.shenyu.admin.model.event.handle.BatchPluginHandleChangedEvent;
import org.apache.shenyu.admin.model.event.handle.PluginHandleChangedEvent;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;

/**
 * PluginHandleEventPublisher.
 */
@Component
public class PluginHandleEventPublisher implements AdminDataModelChangedEventPublisher<PluginHandleDO> {
    
    private final ApplicationEventPublisher publisher;
    
    public PluginHandleEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }
    
    /**
     * on pluginHandle created.
     *
     * @param pluginHandle pluginHandle
     */
    @Override
    public void onCreated(final PluginHandleDO pluginHandle) {
        publish(new PluginHandleChangedEvent(pluginHandle, null, EventTypeEnum.PLUGIN_HANDLE_CREATE, SessionUtil.visitorName()));
    }
    
    /**
     * on pluginHandle updated.
     *
     * @param pluginHandle pluginHandle
     * @param before before pluginHandle
     */
    @Override
    public void onUpdated(final PluginHandleDO pluginHandle, final PluginHandleDO before) {
        publish(new PluginHandleChangedEvent(pluginHandle, before, EventTypeEnum.PLUGIN_HANDLE_UPDATE, SessionUtil.visitorName()));
    }
    
    /**
     * on plugin deleted.
     *
     * @param plugin plugin
     */
    @Override
    public void onDeleted(final PluginHandleDO plugin) {
        publish(new PluginHandleChangedEvent(plugin, null, EventTypeEnum.PLUGIN_HANDLE_DELETE, SessionUtil.visitorName()));
    }
    
    /**
     * on plugin deleted.
     *
     * @param plugins plugins
     */
    @Override
    public void onDeleted(final Collection<PluginHandleDO> plugins) {
        publish(new BatchPluginHandleChangedEvent(plugins, null, EventTypeEnum.PLUGIN_HANDLE_DELETE, SessionUtil.visitorName()));
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
