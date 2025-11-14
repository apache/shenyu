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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.service.support.AiProxyRealKeyResolver;
import org.apache.shenyu.admin.service.AiProxyConnectionService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;
import java.util.Objects;

/**
 * Invalidate AiProxy real-key resolver cache only when aiProxy selector changes.
 */
@Component
public class AiProxySelectorResolverInvalidator implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxySelectorResolverInvalidator.class);

    private final AiProxyRealKeyResolver resolver;

    private final AiProxyConnectionService aiProxyConnectionService;

    public AiProxySelectorResolverInvalidator(final AiProxyRealKeyResolver resolver,
                                              final AiProxyConnectionService aiProxyConnectionService) {
        this.resolver = resolver;
        this.aiProxyConnectionService = aiProxyConnectionService;
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        for (SelectorData d : changed) {
            if (Objects.isNull(d) || Objects.isNull(d.getId())) {
                continue;
            }
            if (!PluginEnum.AI_PROXY.getName().equals(d.getPluginName())) {
                continue;
            }
            resolver.invalidate(d.getId());
            LOG.info("[AiProxyResolverInvalidator] invalidated selectorId={} due to {} (selector)", d.getId(), eventType);
            try {
                if (TransactionSynchronizationManager.isSynchronizationActive()) {
                    TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                        @Override
                        public void afterCommit() {
                            aiProxyConnectionService.refreshApiKeysBySelectorId(d.getId());
                        }
                    });
                } else {
                    aiProxyConnectionService.refreshApiKeysBySelectorId(d.getId());
                }
            } catch (Exception ex) {
                LOG.warn("[AiProxyResolverInvalidator] failed to refresh AI_PROXY_API_KEY for selectorId={}: {}", d.getId(), ex.getMessage());
            }
        }
    }

    @Override
    public void onProxySelectorChanged(final List<ProxySelectorData> changed, final DataEventTypeEnum eventType) {
        if (CollectionUtils.isEmpty(changed)) {
            return;
        }
        for (ProxySelectorData d : changed) {
            if (Objects.isNull(d) || Objects.isNull(d.getId())) {
                continue;
            }
            if (!PluginEnum.AI_PROXY.getName().equals(d.getPluginName())) {
                continue;
            }
            resolver.invalidate(d.getId());
            LOG.info("[AiProxyResolverInvalidator] invalidated selectorId={} due to {} (proxy selector)", d.getId(), eventType);
            try {
                if (TransactionSynchronizationManager.isSynchronizationActive()) {
                    TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                        @Override
                        public void afterCommit() {
                            aiProxyConnectionService.refreshApiKeysBySelectorId(d.getId());
                        }
                    });
                } else {
                    aiProxyConnectionService.refreshApiKeysBySelectorId(d.getId());
                }
            } catch (Exception ex) {
                LOG.warn("[AiProxyResolverInvalidator] failed to refresh AI_PROXY_API_KEY for selectorId={}: {}", d.getId(), ex.getMessage());
            }
        }
    }
} 