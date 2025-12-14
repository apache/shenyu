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

package org.apache.shenyu.admin.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AiProxyApiKeyMapper;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.service.AiProxyConnectionService;
import org.apache.shenyu.admin.service.support.AiProxyRealKeyResolver;
import org.apache.shenyu.admin.utils.NamespaceUtils;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * The type Ai proxy connection service.
 */
@Service
public class AiProxyConnectionServiceImpl implements AiProxyConnectionService {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyConnectionServiceImpl.class);

    private final AiProxyRealKeyResolver aiProxyRealKeyResolver;

    private final AiProxyApiKeyMapper aiProxyApiKeyMapper;

    private final ApplicationEventPublisher eventPublisher;

    public AiProxyConnectionServiceImpl(final AiProxyRealKeyResolver aiProxyRealKeyResolver,
            final AiProxyApiKeyMapper aiProxyApiKeyMapper,
            final ApplicationEventPublisher eventPublisher) {
        this.aiProxyRealKeyResolver = aiProxyRealKeyResolver;
        this.aiProxyApiKeyMapper = aiProxyApiKeyMapper;
        this.eventPublisher = eventPublisher;
    }

    @Override
    public void refreshApiKeysBySelectorId(final String selectorId) {
        // 1. Invalidate resolver cache to ensure next step fetches the new key from
        // selector's handle
        aiProxyRealKeyResolver.invalidate(selectorId);
        LOG.info("[AiProxyConnectionService] invalidated real-key resolver for selectorId={}", selectorId);

        // 2. Find all proxy api keys associated with this selector
        List<ProxyApiKeyDO> keys = aiProxyApiKeyMapper.selectBySelectorId(selectorId);
        if (CollectionUtils.isEmpty(keys)) {
            LOG.info("[AiProxyConnectionService] no api keys found for selectorId={}, skipping refresh", selectorId);
            return;
        }

        // 3. Convert to ProxyApiKeyData, which will resolve the new realApiKey
        List<ProxyApiKeyData> apiKeyDataList = keys.stream()
                .map(this::buildData)
                .collect(Collectors.toList());

        // 4. Publish an UPDATE event to sync data to gateway
        eventPublisher.publishEvent(
                new DataChangedEvent(ConfigGroupEnum.AI_PROXY_API_KEY, DataEventTypeEnum.UPDATE, apiKeyDataList));
        LOG.info("[AiProxyConnectionService] published UPDATE event for {} api keys under selectorId={}",
                apiKeyDataList.size(), selectorId);
    }

    private ProxyApiKeyData buildData(final ProxyApiKeyDO apiKeyDO) {
        String realApiKey = aiProxyRealKeyResolver.resolveRealKey(apiKeyDO.getSelectorId()).orElse(null);
        String normalizedNs = NamespaceUtils.normalizeNamespace(apiKeyDO.getNamespaceId());
        ProxyApiKeyData data = new ProxyApiKeyData();
        data.setSelectorId(apiKeyDO.getSelectorId());
        data.setNamespaceId(normalizedNs);
        data.setProxyApiKey(apiKeyDO.getProxyApiKey());
        data.setRealApiKey(realApiKey);
        data.setEnabled(Boolean.TRUE.equals(apiKeyDO.getEnabled()));
        data.setDescription(apiKeyDO.getDescription());
        return data;
    }

}