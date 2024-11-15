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

package org.apache.shenyu.admin.service.register;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.stereotype.Service;

/**
 * spring mvc websocket service register.
 */
@Service
public class ShenyuClientRegisterWebSocketServiceImpl extends AbstractContextPathRegisterService {

    @Override
    public String rpcType() {
        return RpcTypeEnum.WEB_SOCKET.getName();
    }

    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return "";
    }

    @Override
    protected String ruleHandler() {
        return new WebSocketRuleHandle().toJson();
    }

    @Override
    protected void registerMetadata(final MetaDataRegisterDTO dto) {
        if (dto.isRegisterMetaData()) {
            MetaDataService metaDataService = getMetaDataService();
            MetaDataDO exist = metaDataService.findByPathAndNamespaceId(dto.getPath(), dto.getNamespaceId());
            metaDataService.saveOrUpdateMetaData(exist, dto);
        }
    }

    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        String handleAdd;
        List<WebSocketUpstream> addList = buildWebSocketUpstreamList(uriList);
        List<WebSocketUpstream> canAddList = new CopyOnWriteArrayList<>();
        List<WebSocketUpstream> existList = GsonUtils.getInstance().fromCurrentList(selectorDO.getHandle(), WebSocketUpstream.class);
        if (CollectionUtils.isEmpty(existList)) {
            handleAdd = GsonUtils.getInstance().toJson(addList);
            canAddList = addList;
        } else {
            List<WebSocketUpstream> diffList = addList.stream().filter(divideUpstream -> !existList.contains(divideUpstream)).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(diffList)) {
                canAddList.addAll(diffList);
                existList.addAll(diffList);
            }
            handleAdd = GsonUtils.getInstance().toJson(existList);
        }
        doSubmit(selectorDO.getId(), canAddList);
        return handleAdd;
    }

    private List<WebSocketUpstream> buildWebSocketUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream()
                .map(dto -> CommonUpstreamUtils.buildWebSocketUpstream(dto.getProtocol(), dto.getHost(), dto.getPort(), dto.getNamespaceId()))
                .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
    }

    @Override
    public String offline(final String selectorName, final List<URIRegisterDTO> offlineList, final String namespaceId) {
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        SelectorService selectorService = getSelectorService();
        SelectorDO selectorDO = selectorService.findByNameAndPluginNameAndNamespaceId(selectorName, pluginName, namespaceId);
        if (Objects.isNull(selectorDO)) {
            return Constants.SUCCESS;
        }
        List<URIRegisterDTO> validOfflineUrl = offlineList.stream()
                .filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost()))
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(validOfflineUrl)) {
            for (URIRegisterDTO offlineUrl : validOfflineUrl) {
                WebSocketUpstream webSocketUpstream = CommonUpstreamUtils.buildWebSocketUpstream(offlineUrl.getProtocol(), offlineUrl.getHost(), offlineUrl.getPort(), offlineUrl.getNamespaceId());
                removeDiscoveryUpstream(selectorDO.getId(), webSocketUpstream.getUrl());
            }
            DiscoverySyncData discoverySyncData = fetch(selectorDO.getId(), selectorDO.getName(), pluginName, selectorDO.getNamespaceId());
            getEventPublisher().publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData)));
        }
        return Constants.SUCCESS;
    }

}
