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

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AiProxyApiKeyMapper;
import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.model.vo.ProxyApiKeyVO;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.admin.service.support.AiProxyRealKeyResolver;
import org.apache.shenyu.admin.transfer.ProxyApiKeyTransfer;
import org.apache.shenyu.admin.utils.NamespaceUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/** Implementation of AiProxyApiKeyService. */
@Service
public class AiProxyApiKeyServiceImpl implements AiProxyApiKeyService {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyApiKeyServiceImpl.class);

    private final AiProxyApiKeyMapper mapper;

    private final ApplicationEventPublisher eventPublisher;

    private final AiProxyRealKeyResolver realKeyResolver;

    @Autowired
    public AiProxyApiKeyServiceImpl(
            final AiProxyApiKeyMapper mapper, final ApplicationEventPublisher eventPublisher,
            final AiProxyRealKeyResolver realKeyResolver) {
        this.mapper = mapper;
        this.eventPublisher = eventPublisher;
        this.realKeyResolver = realKeyResolver;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int create(final ProxyApiKeyDTO dto, final String selectorId) {
        final ProxyApiKeyDO entity = ProxyApiKeyTransfer.INSTANCE.mapToEntity(dto);
        if (StringUtils.isBlank(entity.getId())) {
            entity.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        if (StringUtils.isBlank(entity.getProxyApiKey())) {
            entity.setProxyApiKey(SignUtils.generateKey());
        }
        // validate namespace
        if (StringUtils.isBlank(entity.getNamespaceId())) {
            throw new ShenyuException(ShenyuResultMessage.PARAMETER_ERROR);
        }
        // validate selector id
        if (StringUtils.isBlank(selectorId)) {
            throw new ShenyuException(ShenyuResultMessage.PARAMETER_ERROR);
        }
        entity.setSelectorId(selectorId);
        // unique check for proxyApiKey if provided
        if (StringUtils.isNotBlank(entity.getProxyApiKey())
                && Boolean.TRUE.equals(mapper.proxyApiKeyExisted(selectorId, entity.getProxyApiKey()))) {
            throw new ShenyuException(ShenyuResultMessage.UNIQUE_INDEX_CONFLICT_ERROR);
        }
        if (Objects.isNull(entity.getEnabled())) {
            entity.setEnabled(Boolean.TRUE);
        }
        // back fill generated fields to response dto first
        dto.setId(entity.getId());
        dto.setProxyApiKey(entity.getProxyApiKey());
        dto.setEnabled(entity.getEnabled());
        final int rows = mapper.insert(entity);
        publishChange(DataEventTypeEnum.CREATE, entity);
        return rows;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int update(final ProxyApiKeyDTO dto) {
        final ProxyApiKeyDO entity = ProxyApiKeyTransfer.INSTANCE.mapToEntity(dto);
        // id is required for update
        if (Objects.isNull(entity) || StringUtils.isBlank(entity.getId())) {
            return 0;
        }
        int rows = mapper.updateSelective(entity);
        publishChange(DataEventTypeEnum.UPDATE, entity);
        return rows;
    }

    @Override
    public ProxyApiKeyVO findById(final String id) {
        final ProxyApiKeyVO vo = ProxyApiKeyTransfer.INSTANCE.mapToVO(mapper.selectById(id));
        if (Objects.nonNull(vo)) {
            final String real = realKeyResolver.resolveRealKey(vo.getSelectorId()).orElse(null);
            vo.setRealApiKey(real);
        }
        return vo;
    }

    @Override
    public List<ProxyApiKeyVO> findByIds(final List<String> ids) {
        List<ProxyApiKeyVO> voList = mapper.selectByIds(ids).stream()
                .map(ProxyApiKeyTransfer.INSTANCE::mapToVO)
                .collect(Collectors.toList());
        // Populate realApiKey for each VO using batch resolver
        List<String> selectorIds = voList.stream()
                .map(ProxyApiKeyVO::getSelectorId)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        if (!selectorIds.isEmpty()) {
            // resolveRealKeys returns Map<String, String> mapping selectorId to realApiKey
            java.util.Map<String, String> realKeyMap = realKeyResolver.resolveRealKeys(selectorIds);
            for (ProxyApiKeyVO vo : voList) {
                if (Objects.nonNull(vo.getSelectorId())) {
                    vo.setRealApiKey(realKeyMap.get(vo.getSelectorId()));
                }
            }
        }
        return voList;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        final List<ProxyApiKeyDO> toDelete = mapper.selectByIds(ids);
        int rows = mapper.deleteByIds(ids);
        if (rows > 0 && CollectionUtils.isNotEmpty(toDelete)) {
            publishChangeList(DataEventTypeEnum.DELETE, toDelete);
        }
        return rows;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String enabled(final List<String> ids, final Boolean enabled) {
        if (CollectionUtils.isEmpty(ids) || Objects.isNull(enabled)) {
            return ShenyuResultMessage.PARAMETER_ERROR;
        }
        int rows = mapper.updateEnableBatch(ids, enabled);
        if (rows > 0) {
            final List<ProxyApiKeyDO> updated = mapper.selectByIds(ids);
            if (Objects.nonNull(updated) && !updated.isEmpty()) {
                for (ProxyApiKeyDO e : updated) {
                    e.setEnabled(enabled);
                }
                publishChangeList(DataEventTypeEnum.UPDATE, updated);
            }
            return StringUtils.EMPTY;
        }
        return AdminConstants.ID_NOT_EXIST;
    }

    @Override
    public CommonPager<ProxyApiKeyVO> listByPage(final ProxyApiKeyQuery query) {
        final int current = query.getPageParameter().getCurrentPage();
        final int size = query.getPageParameter().getPageSize();
        PageHelper.startPage(current, size);
        final List<ProxyApiKeyVO> list = mapper.selectByCondition(query);

        if (CollectionUtils.isNotEmpty(list)) {
            java.util.Set<String> selectorIds = list.stream().map(ProxyApiKeyVO::getSelectorId)
                    .collect(Collectors.toSet());
            java.util.Map<String, String> realKeys = realKeyResolver.resolveRealKeys(selectorIds);
            for (ProxyApiKeyVO vo : list) {
                vo.setRealApiKey(realKeys.get(vo.getSelectorId()));
            }
        }

        final PageInfo<ProxyApiKeyVO> pageInfo = new PageInfo<>(list);
        return new CommonPager<>(new PageParameter(current, size, (int) pageInfo.getTotal()), list);
    }

    @Override
    public List<ProxyApiKeyVO> searchByCondition(final ProxyApiKeyQuery condition) {
        return mapper.selectByCondition(condition);
    }

    // sync & listAll

    @Override
    public List<ProxyApiKeyData> listAll() {
        List<ProxyApiKeyDO> all = mapper.selectAll();
        if (CollectionUtils.isEmpty(all)) {
            return java.util.Collections.emptyList();
        }
        java.util.Set<String> selectorIds = all.stream().map(ProxyApiKeyDO::getSelectorId).collect(Collectors.toSet());
        java.util.Map<String, String> realKeys = realKeyResolver.resolveRealKeys(selectorIds);

        List<ProxyApiKeyData> list = all.stream()
                .map(e -> convert(e, realKeys.get(e.getSelectorId())))
                .collect(Collectors.toList());
        LOG.info("[AiProxySync] listAll size:{}", list.size());
        return list;
    }

    @Override
    public void syncData() {
        // group by namespace and publish REFRESH respectively
        List<ProxyApiKeyDO> all = mapper.selectAll();
        if (CollectionUtils.isEmpty(all)) {
            return;
        }
        LOG.info("[AiProxySync] syncData triggered, total records:{}", all.size());

        java.util.Set<String> selectorIds = all.stream().map(ProxyApiKeyDO::getSelectorId).collect(Collectors.toSet());
        java.util.Map<String, String> realKeys = realKeyResolver.resolveRealKeys(selectorIds);

        all.stream()
                .collect(Collectors.groupingBy(ProxyApiKeyDO::getNamespaceId))
                .values()
                .forEach(list -> publishRefresh(list, realKeys));
    }

    @Override
    public void syncDataByNamespaceId(final String namespaceId) {
        final String target = NamespaceUtils.normalizeNamespace(namespaceId);
        List<ProxyApiKeyDO> all = mapper.selectAll();
        List<ProxyApiKeyDO> list = Objects.isNull(all) ? java.util.Collections.emptyList()
                : all.stream()
                        .filter(e -> StringUtils.equals(NamespaceUtils.normalizeNamespace(e.getNamespaceId()), target))
                        .collect(Collectors.toList());
        LOG.info("[AiProxySync] syncDataByNamespaceId {}, normalized:{}, matched:{} of total:{}",
                namespaceId, target, list.size(), Objects.isNull(all) ? 0 : all.size());
        if (list.isEmpty()) {
            return;
        }
        publishRefresh(list, realKeyResolver
                .resolveRealKeys(list.stream().map(ProxyApiKeyDO::getSelectorId).collect(Collectors.toSet())));
    }

    // private utils

    private void publishChange(final DataEventTypeEnum type, final ProxyApiKeyDO entity) {
        if (Objects.isNull(eventPublisher) || Objects.isNull(entity)) {
            return;
        }
        final ProxyApiKeyData data = convert(entity);
        eventPublisher.publishEvent(new DataChangedEvent(
                ConfigGroupEnum.AI_PROXY_API_KEY,
                type,
                java.util.Collections.singletonList(data)));
        final String ns = data.getNamespaceId();
        LOG.info("[AiProxySync] publish {}, size=1, namespaceId={}", type, ns);
        if (LOG.isDebugEnabled()) {
            String masked = Objects.isNull(entity.getProxyApiKey()) ? null
                    : entity.getProxyApiKey().substring(0, Math.min(6, entity.getProxyApiKey().length()));
            LOG.debug("[AiProxySync] published {}, proxyKeyMask={}..., ns={}", type, masked, ns);
        }
    }

    private void publishChangeList(
            final DataEventTypeEnum type, final List<ProxyApiKeyDO> entities) {
        if (Objects.isNull(eventPublisher) || Objects.isNull(entities) || entities.isEmpty()) {
            return;
        }
        final List<ProxyApiKeyData> dataList = entities.stream().map(this::convert).collect(Collectors.toList());
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.AI_PROXY_API_KEY, type, dataList));
        // summarize namespaces for info log
        final java.util.Set<String> nsSet = dataList.stream().map(ProxyApiKeyData::getNamespaceId)
                .filter(Objects::nonNull).collect(Collectors.toSet());
        final String nsSummary = nsSet.isEmpty() ? "unknown" : (nsSet.size() == 1 ? nsSet.iterator().next() : "multi");
        LOG.info("[AiProxySync] publish {}, size={}, namespace={} ", type, dataList.size(), nsSummary);
        if (LOG.isDebugEnabled()) {
            dataList.stream().map(ProxyApiKeyData::getProxyApiKey).filter(Objects::nonNull).forEach(k -> {
                String masked = k.substring(0, Math.min(6, k.length()));
                LOG.debug("[AiProxySync] published {}, proxyKeyMask={}...", type, masked);
            });
        }
    }

    private void publishRefresh(final List<ProxyApiKeyDO> entities, final java.util.Map<String, String> realKeys) {
        List<ProxyApiKeyData> dataList = entities.stream()
                .map(e -> convert(e, realKeys.get(e.getSelectorId())))
                .collect(Collectors.toList());
        eventPublisher.publishEvent(new DataChangedEvent(
                ConfigGroupEnum.AI_PROXY_API_KEY,
                DataEventTypeEnum.REFRESH,
                dataList));
    }

    private ProxyApiKeyData convert(final ProxyApiKeyDO entity) {
        final String normalizedNs = NamespaceUtils.normalizeNamespace(entity.getNamespaceId());
        if (!StringUtils.equals(entity.getNamespaceId(), normalizedNs)) {
            LOG.info("[AiProxySync] normalize namespace from {} to {} for proxyKey={}",
                    entity.getNamespaceId(), normalizedNs, entity.getProxyApiKey());
        }
        final String resolvedRealKey = realKeyResolver.resolveRealKey(entity.getSelectorId()).orElse(null);
        return convert(entity, resolvedRealKey);
    }

    private ProxyApiKeyData convert(final ProxyApiKeyDO entity, final String resolvedRealKey) {
        final String normalizedNs = NamespaceUtils.normalizeNamespace(entity.getNamespaceId());
        if (!StringUtils.equals(entity.getNamespaceId(), normalizedNs)) {
            LOG.info("[AiProxySync] normalize namespace from {} to {} for proxyKey={}",
                    entity.getNamespaceId(), normalizedNs, entity.getProxyApiKey());
        }
        return ProxyApiKeyData.builder()
                .proxyApiKey(entity.getProxyApiKey())
                .realApiKey(resolvedRealKey)
                .description(entity.getDescription())
                .enabled(entity.getEnabled())
                .namespaceId(normalizedNs)
                .selectorId(entity.getSelectorId())
                .build();
    }
}
