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
import org.apache.shenyu.admin.model.event.plugin.BatchNamespacePluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.BatchNamespacePluginDeletedEvent;
import org.apache.shenyu.admin.model.event.plugin.NamespacePluginChangedEvent;
import org.apache.shenyu.admin.model.event.plugin.NamespacePluginCreatedEvent;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * NamespacePluginEventPublisher.
 */
@Component
public class NamespacePluginEventPublisher implements AdminDataModelChangedEventPublisher<NamespacePluginVO> {

    private final ApplicationEventPublisher publisher;

    public NamespacePluginEventPublisher(final ApplicationEventPublisher publisher) {
        this.publisher = publisher;
    }


    /**
     * on plugin created.
     *
     * @param namespacePluginVO namespacePluginVO
     */
    @Override
    public void onCreated(final NamespacePluginVO namespacePluginVO) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.CREATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(namespacePluginVO))));
        publish(new NamespacePluginCreatedEvent(namespacePluginVO, SessionUtil.visitorName()));
    }

    @Override
    public void onUpdated(final NamespacePluginVO namespacePluginVO, final NamespacePluginVO before) {
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(namespacePluginVO))));
        publish(new NamespacePluginChangedEvent(namespacePluginVO, before, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
    }

    @Override
    public void onDeleted(final NamespacePluginVO namespacePluginVO) {
        publish(new NamespacePluginChangedEvent(namespacePluginVO, null, EventTypeEnum.PLUGIN_DELETE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                Stream.of(namespacePluginVO).map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }

    @Override
    public void onDeleted(final Collection<NamespacePluginVO> namespacePlugin) {
        String namespaceId = ((Collection<?>) namespacePlugin)
                .stream()
                .map(NamespacePluginVO.class::cast)
                .findFirst()
                .map(NamespacePluginVO::getNamespaceId)
                .orElse(SYS_DEFAULT_NAMESPACE_ID);
        List<PluginDO> pluginDOList = ((Collection<?>) namespacePlugin)
                .stream()
                .map(NamespacePluginVO.class::cast)
                .map(PluginDO::buildPluginDO)
                .collect(Collectors.toList());
        publish(new BatchNamespacePluginDeletedEvent(pluginDOList, SessionUtil.visitorName(), namespaceId));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.DELETE,
                namespacePlugin.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));

    }

    /**
     * on plugin namespace batch enabled.
     *
     * @param namespacePlugin namespacePlugin
     */
    public void onEnabled(final Collection<NamespacePluginVO> namespacePlugin) {
        publish(new BatchNamespacePluginChangedEvent(namespacePlugin, null, EventTypeEnum.PLUGIN_UPDATE, SessionUtil.visitorName()));
        publisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                namespacePlugin.stream().map(PluginTransfer.INSTANCE::mapToData).collect(Collectors.toList())));
    }

    @Override
    public void publish(final AdminDataModelChangedEvent event) {
        publisher.publishEvent(event);
    }
}
