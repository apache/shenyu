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

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.register.common.protocol.RegistrationFencingAddress;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.admin.utils.DiscoveryUpstreamPropsValidator;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.function.Function;
import java.util.function.Consumer;

@Service
public class DiscoveryUpstreamServiceImpl implements DiscoveryUpstreamService {

    private static final Logger LOG = LoggerFactory.getLogger(DiscoveryUpstreamServiceImpl.class);

    // Keep IN lists below Oracle's 1000-expression limit and bound query payloads on all databases.
    private static final int BATCH_SIZE = 500;

    // free-form props key carrying the draining admission marker (#8042)
    private static final String DRAINING_PROP_KEY = "draining";

    // free-form props key carrying the not-ready admission marker (#8042)
    private static final String NOT_READY_PROP_KEY = "notReady";

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final DiscoveryRelMapper discoveryRelMapper;

    private final ProxySelectorMapper proxySelectorMapper;

    private final DiscoveryMapper discoveryMapper;

    private final PluginMapper pluginMapper;

    private final SelectorMapper selectorMapper;

    private final DiscoveryProcessorHolder discoveryProcessorHolder;

    private final RegistrationFencingGuard registrationFencingGuard;

    public DiscoveryUpstreamServiceImpl(final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                        final DiscoveryHandlerMapper discoveryHandlerMapper,
                                        final ProxySelectorMapper proxySelectorMapper,
                                        final DiscoveryMapper discoveryMapper,
                                        final DiscoveryRelMapper discoveryRelMapper,
                                        final SelectorMapper selectorMapper,
                                        final PluginMapper pluginMapper,
                                        final DiscoveryProcessorHolder discoveryProcessorHolder,
                                        final RegistrationFencingGuard registrationFencingGuard) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryProcessorHolder = discoveryProcessorHolder;
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.discoveryMapper = discoveryMapper;
        this.discoveryRelMapper = discoveryRelMapper;
        this.selectorMapper = selectorMapper;
        this.proxySelectorMapper = proxySelectorMapper;
        this.pluginMapper = pluginMapper;
        this.registrationFencingGuard = registrationFencingGuard;
    }

    /**
     * createOrUpdate.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {
        List<String> ids = StringUtils.hasLength(discoveryUpstreamDTO.getId())
                ? Collections.singletonList(discoveryUpstreamDTO.getId()) : Collections.emptyList();
        Map<String, DiscoveryUpstreamDO> existingRows = lockAndAssertUpstreamWritesAllowed(ids,
                Collections.singletonList(discoveryUpstreamDTO.getDiscoveryHandlerId()));
        return StringUtils.hasLength(discoveryUpstreamDTO.getId())
                ? update(discoveryUpstreamDTO, existingRows.get(discoveryUpstreamDTO.getId())) : create(discoveryUpstreamDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void createOrUpdateBatch(final List<DiscoveryUpstreamDTO> upstreams) {
        if (CollectionUtils.isEmpty(upstreams)) {
            return;
        }
        upstreams.forEach(upstream -> DiscoveryUpstreamPropsValidator.validate(upstream.getProps()));
        Set<String> handlerIds = new LinkedHashSet<>();
        List<String> upstreamIds = upstreams.stream().map(DiscoveryUpstreamDTO::getId)
                .filter(StringUtils::hasLength).collect(Collectors.toList());
        List<String> destinationHandlerIds = upstreams.stream().map(DiscoveryUpstreamDTO::getDiscoveryHandlerId)
                .collect(Collectors.toList());
        Map<String, DiscoveryUpstreamDO> existingRows = lockAndAssertUpstreamWritesAllowed(upstreamIds, destinationHandlerIds);
        boolean ownerChanged = false;
        for (DiscoveryUpstreamDTO upstream : upstreams) {
            DiscoveryUpstreamDO entity = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(upstream);
            if (StringUtils.hasLength(upstream.getId())) {
                DiscoveryUpstreamDO existing = existingRows.get(upstream.getId());
                if (Objects.nonNull(existing) && discoveryUpstreamMapper.updateForHandler(entity, existing.getDiscoveryHandlerId()) != 1) {
                    throw new IllegalStateException("Discovery upstream owner changed during update");
                }
                if (Objects.nonNull(existing) && StringUtils.hasLength(existing.getDiscoveryHandlerId())) {
                    handlerIds.add(existing.getDiscoveryHandlerId());
                    ownerChanged |= !Objects.equals(existing.getDiscoveryHandlerId(), upstream.getDiscoveryHandlerId());
                }
            } else {
                discoveryUpstreamMapper.insert(entity);
            }
            handlerIds.add(upstream.getDiscoveryHandlerId());
        }
        if (ownerChanged) {
            fetchAllAfterCommit(handlerIds);
        } else {
            handlerIds.forEach(this::fetchAll);
        }
    }

    @Override
    @Transactional
    public int updateBatch(final String discoveryHandlerId, final List<DiscoveryUpstreamDTO> discoveryUpstreamDTOList) {
        // Validate the complete replacement before deleting anything or emitting a sync event.
        discoveryUpstreamDTOList.forEach(upstream -> DiscoveryUpstreamPropsValidator.validate(upstream.getProps()));
        lockAndAssertUpstreamWritesAllowed(Collections.emptyList(), Collections.singletonList(discoveryHandlerId));
        discoveryUpstreamMapper.deleteByDiscoveryHandlerId(discoveryHandlerId);
        for (DiscoveryUpstreamDTO discoveryUpstreamDTO : discoveryUpstreamDTOList) {
            discoveryUpstreamDTO.setId(null);
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
            discoveryUpstreamDO.setDiscoveryHandlerId(discoveryHandlerId);
            discoveryUpstreamMapper.insert(discoveryUpstreamDO);
        }
        fetchAfterCommit(discoveryHandlerId);
        return 0;
    }

    private void fetchAfterCommit(final String discoveryHandlerId) {
        fetchAllAfterCommit(Collections.singletonList(discoveryHandlerId));
    }

    private void fetchAllAfterCommit(final Collection<String> discoveryHandlerIds) {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    discoveryHandlerIds.stream().filter(StringUtils::hasLength).distinct().forEach(DiscoveryUpstreamServiceImpl.this::fetchAll);
                }
            });
        } else {
            discoveryHandlerIds.stream().filter(StringUtils::hasLength).distinct().forEach(this::fetchAll);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void nativeCreateOrUpdate(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {
        DiscoveryUpstreamPropsValidator.validate(discoveryUpstreamDTO.getProps());
        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
        String canonicalUrl = canonicalEndpoint(discoveryUpstreamDO.getUpstreamUrl());
        List<String> ids = StringUtils.hasLength(discoveryUpstreamDTO.getId())
                ? Collections.singletonList(discoveryUpstreamDTO.getId()) : Collections.emptyList();
        Map<String, DiscoveryUpstreamDO> existingRows = lockAndAssertUpstreamWritesAllowed(ids,
                Collections.singletonList(discoveryUpstreamDO.getDiscoveryHandlerId()), false);
        assertHandlerAddressWriteAllowed(discoveryUpstreamDO.getDiscoveryHandlerId(), canonicalUrl);
        if (StringUtils.hasLength(discoveryUpstreamDTO.getId())) {
            DiscoveryUpstreamDO existingRecord = existingRows.get(discoveryUpstreamDTO.getId());
            if (Objects.nonNull(existingRecord)) {
                assertHandlerAddressWriteAllowed(existingRecord.getDiscoveryHandlerId(), existingRecord.getUpstreamUrl());
                discoveryUpstreamDO.setUpstreamUrl(canonicalUrl);
                if (discoveryUpstreamMapper.updateSelectiveForHandler(discoveryUpstreamDO, existingRecord.getDiscoveryHandlerId()) != 1) {
                    throw new IllegalStateException("Discovery upstream owner changed during native update");
                }
            }
        } else {
            DiscoveryUpstreamDO existingRecord = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryUpstreamDO.getDiscoveryHandlerId())
                    .stream().filter(record -> Objects.equals(canonicalUrl, canonicalEndpoint(record.getUpstreamUrl())))
                    .findFirst().orElse(null);
            if (Objects.isNull(existingRecord)) {
                discoveryUpstreamMapper.insert(discoveryUpstreamDO);
            } else if (!Objects.equals(existingRecord.getUpstreamUrl(), canonicalUrl)) {
                existingRecord.setUpstreamUrl(canonicalUrl);
                discoveryUpstreamMapper.updateSelective(existingRecord);
            }
        }
    }

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // resolve the owning handlers before the rows disappear, so the gateways can be
        // notified with the remaining upstream snapshot (including an empty one)
        if (CollectionUtils.isEmpty(ids)) {
            return ShenyuResultMessage.DELETE_SUCCESS;
        }
        final Map<String, DiscoveryUpstreamDO> deletedById = lockAndAssertUpstreamWritesAllowed(ids, Collections.emptyList());
        final List<DiscoveryUpstreamDO> deleted = new ArrayList<>(deletedById.values());
        if (deleted.isEmpty()) {
            return ShenyuResultMessage.DELETE_SUCCESS;
        }
        discoveryUpstreamMapper.deleteByIds(deletedById.keySet().stream().sorted().collect(Collectors.toList()));
        deleted.stream().map(DiscoveryUpstreamDO::getDiscoveryHandlerId).distinct().forEach(this::fetchAll);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    @Override
    public List<DiscoverySyncData> listAll() {
        return buildSyncData(discoveryHandlerMapper.selectAll());
    }

    @Override
    public List<DiscoverySyncData> listAllByNamespaceId(final String namespaceId) {
        return buildSyncData(discoveryHandlerMapper.selectAllByNamespaceId(namespaceId));
    }

    private List<DiscoverySyncData> buildSyncData(final List<DiscoveryHandlerDO> handlers) {
        List<DiscoverySyncData> result = new ArrayList<>();
        for (List<DiscoveryHandlerDO> batch : Lists.partition(handlers, BATCH_SIZE)) {
            List<String> handlerIds = batch.stream().map(DiscoveryHandlerDO::getId).collect(Collectors.toList());
            List<DiscoveryRelDO> relations = discoveryRelMapper.selectByDiscoveryHandlerIds(handlerIds);
            Set<String> selectorIds = relations.stream().map(DiscoveryRelDO::getSelectorId).filter(StringUtils::hasLength).collect(Collectors.toSet());
            List<String> proxyIds = relations.stream().filter(rel -> !StringUtils.hasLength(rel.getSelectorId()))
                    .map(DiscoveryRelDO::getProxySelectorId).filter(StringUtils::hasLength).distinct().collect(Collectors.toList());
            Map<String, SelectorDO> selectors = selectorIds.isEmpty() ? Collections.emptyMap()
                    : selectorMapper.selectByIdSet(selectorIds).stream().collect(Collectors.toMap(SelectorDO::getId, Function.identity()));
            Map<String, ProxySelectorDO> proxies = proxyIds.isEmpty() ? Collections.emptyMap()
                    : proxySelectorMapper.selectByIds(proxyIds).stream().collect(Collectors.toMap(ProxySelectorDO::getId, Function.identity()));
            Map<String, List<DiscoveryUpstreamData>> upstreams = discoveryUpstreamMapper.selectByDiscoveryHandlerIds(handlerIds).stream()
                    .collect(Collectors.groupingBy(DiscoveryUpstreamDO::getDiscoveryHandlerId,
                            Collectors.mapping(DiscoveryTransfer.INSTANCE::mapToData, Collectors.toList())));
            Map<String, DiscoveryRelDO> relationByHandler = relations.stream()
                    .collect(Collectors.toMap(DiscoveryRelDO::getDiscoveryHandlerId, Function.identity(), (existing, duplicate) -> {
                        LOG.warn("Duplicate discovery relations for handler {}, retaining relation {} and ignoring {}",
                                existing.getDiscoveryHandlerId(), existing.getId(), duplicate.getId());
                        return existing;
                    }));
            for (DiscoveryHandlerDO handler : batch) {
                DiscoveryRelDO relation = relationByHandler.get(handler.getId());
                if (Objects.isNull(relation)) {
                    LOG.warn("Skipping discovery handler {}: relation is missing", handler.getId());
                    continue;
                }
                DiscoverySyncData data = new DiscoverySyncData();
                data.setPluginName(relation.getPluginName());
                if (StringUtils.hasLength(relation.getSelectorId())) {
                    SelectorDO selector = selectors.get(relation.getSelectorId());
                    if (Objects.isNull(selector)) {
                        LOG.warn("Skipping discovery handler {}: selector {} is missing", handler.getId(), relation.getSelectorId());
                        continue;
                    }
                    data.setSelectorId(selector.getId());
                    data.setSelectorName(selector.getSelectorName());
                    data.setNamespaceId(selector.getNamespaceId());
                } else {
                    ProxySelectorDO proxy = proxies.get(relation.getProxySelectorId());
                    if (Objects.isNull(proxy)) {
                        LOG.warn("Skipping discovery handler {}: proxy selector {} is missing", handler.getId(), relation.getProxySelectorId());
                        continue;
                    }
                    data.setSelectorId(proxy.getId());
                    data.setSelectorName(proxy.getName());
                    data.setNamespaceId(proxy.getNamespaceId());
                }
                data.setUpstreamDataList(upstreams.getOrDefault(handler.getId(), Collections.emptyList()));
                result.add(data);
            }
        }
        return result;
    }

    @Override
    public List<DiscoveryUpstreamVO> listAllData() {
        return discoveryUpstreamMapper
                .selectAll()
                .stream()
                .map(DiscoveryTransfer.INSTANCE::mapToVo)
                .collect(Collectors.toList());
    }
    
    @Override
    public List<DiscoveryUpstreamVO> listAllDataByNamespaceId(final String namespaceId) {
        return discoveryUpstreamMapper
                .selectByNamespaceId(namespaceId)
                .stream()
                .map(DiscoveryTransfer.INSTANCE::mapToVo)
                .collect(Collectors.toList());
    }
    
    @Override
    public void refreshBySelectorId(final String selectorId) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selectorId);
        if (Objects.nonNull(discoveryHandlerDO)) {
            fetchAll(discoveryHandlerDO.getId());
        }
    }

    /**
     * create.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    private String create(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {
        DiscoveryUpstreamPropsValidator.validate(discoveryUpstreamDTO.getProps());
        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
        discoveryUpstreamMapper.insert(discoveryUpstreamDO);
        fetchAll(discoveryUpstreamDTO.getDiscoveryHandlerId());
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    /**
     * update.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    private String update(final DiscoveryUpstreamDTO discoveryUpstreamDTO, final DiscoveryUpstreamDO existing) {
        DiscoveryUpstreamPropsValidator.validate(discoveryUpstreamDTO.getProps());
        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
        if (Objects.nonNull(existing) && discoveryUpstreamMapper.updateForHandler(discoveryUpstreamDO, existing.getDiscoveryHandlerId()) != 1) {
            throw new IllegalStateException("Discovery upstream owner changed during update");
        }
        Set<String> affectedHandlerIds = new LinkedHashSet<>();
        boolean ownerChanged = false;
        if (Objects.nonNull(existing) && StringUtils.hasLength(existing.getDiscoveryHandlerId())) {
            affectedHandlerIds.add(existing.getDiscoveryHandlerId());
            ownerChanged = !Objects.equals(existing.getDiscoveryHandlerId(), discoveryUpstreamDTO.getDiscoveryHandlerId());
        }
        affectedHandlerIds.add(discoveryUpstreamDTO.getDiscoveryHandlerId());
        if (ownerChanged) {
            fetchAllAfterCommit(affectedHandlerIds);
        } else {
            affectedHandlerIds.forEach(this::fetchAll);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    @Override
    public List<DiscoveryUpstreamData> findBySelectorId(final String selectorId) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selectorId);
        if (Objects.isNull(discoveryHandlerDO)) {
            return Collections.emptyList();
        }
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDO.getId());
        return discoveryUpstreamDOS.stream().map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
    }

    @Override
    public boolean hasDiscoveryHandler(final String selectorId) {
        return Objects.nonNull(discoveryHandlerMapper.selectBySelectorId(selectorId));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBySelectorIdAndUrl(final String selectorId, final String url) {
        registrationFencingGuard.assertSelectorAddressWriteAllowed(selectorId, url);
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selectorId);
        if (Objects.nonNull(discoveryHandlerDO)) {
            DiscoveryUpstreamDO upstream = findByCanonicalUrl(discoveryHandlerDO.getId(), url);
            if (Objects.isNull(upstream)) {
                return;
            }
            if (Objects.equals(upstream.getUpstreamUrl(), url)) {
                discoveryUpstreamMapper.deleteByUrl(discoveryHandlerDO.getId(), url);
            } else if (StringUtils.hasLength(upstream.getId())) {
                discoveryUpstreamMapper.deleteByIds(Collections.singletonList(upstream.getId()));
            } else {
                // A legacy row without an id cannot be deleted by identity;
                // retain the old URL delete as the safest fallback.
                discoveryUpstreamMapper.deleteByUrl(discoveryHandlerDO.getId(), upstream.getUpstreamUrl());
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void changeStatusBySelectorIdAndUrl(final String selectorId, final String url, final Boolean enabled) {
        registrationFencingGuard.assertSelectorAddressWriteAllowed(selectorId, url);
        changeStatus(selectorId, url, enabled);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean tryChangeStatusBySelectorIdAndUrl(final String selectorId, final String url, final Boolean enabled) {
        if (registrationFencingGuard.isSelectorAddressProtected(selectorId, url)) {
            return false;
        }
        changeStatus(selectorId, url, enabled);
        return true;
    }

    private void changeStatus(final String selectorId, final String url, final Boolean enabled) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selectorId);
        if (Objects.nonNull(discoveryHandlerDO)) {
            DiscoveryUpstreamDO upstream = findByCanonicalUrl(discoveryHandlerDO.getId(), url);
            if (Objects.isNull(upstream)) {
                return;
            }
            if (Objects.equals(upstream.getUpstreamUrl(), url)) {
                discoveryUpstreamMapper.updateStatusByUrl(discoveryHandlerDO.getId(), url, enabled ? 0 : 1);
            } else {
                upstream.setUpstreamStatus(enabled ? 0 : 1);
                upstream.setDateUpdated(new java.sql.Timestamp(System.currentTimeMillis()));
                discoveryUpstreamMapper.updateSelective(upstream);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void markDrainingBySelectorIdAndUrl(final String selectorId, final String url, final boolean draining) {
        registrationFencingGuard.assertSelectorAddressWriteAllowed(selectorId, url);
        updateUpstreamProps(selectorId, url, props -> props.put(DRAINING_PROP_KEY, draining));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void markNotReadyBySelectorIdAndUrl(final String selectorId, final String url, final boolean notReady) {
        registrationFencingGuard.assertSelectorAddressWriteAllowed(selectorId, url);
        updateUpstreamProps(selectorId, url, props -> props.put(NOT_READY_PROP_KEY, notReady));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void markReadyBySelectorIdAndUrl(final String selectorId, final String url) {
        registrationFencingGuard.assertSelectorAddressWriteAllowed(selectorId, url);
        updateUpstreamProps(selectorId, url, props -> {
            props.remove(DRAINING_PROP_KEY);
            props.remove(NOT_READY_PROP_KEY);
        });
    }

    private void updateUpstreamProps(final String selectorId, final String url, final Consumer<Map<String, Object>> mutation) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId(selectorId);
        if (Objects.isNull(discoveryHandlerDO)) {
            return;
        }
        DiscoveryUpstreamDO upstream = findByCanonicalUrl(discoveryHandlerDO.getId(), url);
        if (Objects.isNull(upstream)) {
            return;
        }
        Map<String, Object> props = !StringUtils.hasLength(upstream.getProps())
                ? new LinkedHashMap<>() : GsonUtils.getInstance().convertToMap(upstream.getProps());
        mutation.accept(props);
        String serializedProps = GsonUtils.getInstance().toJson(props);
        if (Objects.equals(upstream.getUpstreamUrl(), url)) {
            discoveryUpstreamMapper.updatePropsByUrl(discoveryHandlerDO.getId(), url, serializedProps);
        } else {
            upstream.setProps(serializedProps);
            upstream.setDateUpdated(new java.sql.Timestamp(System.currentTimeMillis()));
            discoveryUpstreamMapper.updateSelective(upstream);
        }
    }

    /**
     * Resolve an upstream by its canonical endpoint, while retaining the
     * exact URL query for the normal path and existing row-lock behavior.
     * Historical rows may contain a different IPv6 spelling or brackets.
     *
     * @param discoveryHandlerId discovery handler id
     * @param url requested endpoint
     * @return matching row or {@code null}
     */
    private DiscoveryUpstreamDO findByCanonicalUrl(final String discoveryHandlerId, final String url) {
        DiscoveryUpstreamDO exact = discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl(discoveryHandlerId, url);
        if (Objects.nonNull(exact)) {
            return exact;
        }
        final String canonical = canonicalEndpoint(url);
        return Optional.ofNullable(discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerId))
                .orElseGet(Collections::emptyList).stream()
                .filter(Objects::nonNull)
                .filter(record -> Objects.equals(canonical, canonicalEndpoint(record.getUpstreamUrl())))
                .findFirst().orElse(null);
    }

    private Map<String, DiscoveryUpstreamDO> lockAndAssertUpstreamWritesAllowed(final Collection<String> upstreamIds,
                                                                                 final Collection<String> destinationHandlerIds) {
        return lockAndAssertUpstreamWritesAllowed(upstreamIds, destinationHandlerIds, true);
    }

    private Map<String, DiscoveryUpstreamDO> lockAndAssertUpstreamWritesAllowed(final Collection<String> upstreamIds,
                                                                                 final Collection<String> destinationHandlerIds,
                                                                                 final boolean assertSelectorWritesAllowed) {
        List<String> sortedIds = upstreamIds.stream().filter(StringUtils::hasLength).distinct().sorted().collect(Collectors.toList());
        List<DiscoveryUpstreamDO> observedRows = sortedIds.isEmpty()
                ? Collections.emptyList() : discoveryUpstreamMapper.selectByIds(sortedIds);
        Map<String, DiscoveryUpstreamDO> observedById = observedRows.stream()
                .collect(Collectors.toMap(DiscoveryUpstreamDO::getId, Function.identity()));
        Set<String> handlerIds = new TreeSet<>();
        destinationHandlerIds.stream().filter(StringUtils::hasLength).forEach(handlerIds::add);
        observedRows.stream().map(DiscoveryUpstreamDO::getDiscoveryHandlerId).filter(StringUtils::hasLength).forEach(handlerIds::add);
        lockAndAssertHandlerWritesAllowed(handlerIds, assertSelectorWritesAllowed);
        List<DiscoveryUpstreamDO> lockedRows = sortedIds.isEmpty()
                ? Collections.emptyList() : discoveryUpstreamMapper.selectByIdsForUpdate(sortedIds);
        Map<String, DiscoveryUpstreamDO> lockedById = new LinkedHashMap<>();
        for (DiscoveryUpstreamDO locked : lockedRows) {
            DiscoveryUpstreamDO observed = observedById.get(locked.getId());
            if (Objects.isNull(observed) || !Objects.equals(observed.getDiscoveryHandlerId(), locked.getDiscoveryHandlerId())) {
                throw new IllegalStateException("Discovery upstream owner changed during update");
            }
            lockedById.put(locked.getId(), locked);
        }
        return lockedById;
    }

    private void lockAndAssertHandlerWritesAllowed(final Collection<String> discoveryHandlerIds,
                                                   final boolean assertSelectorWritesAllowed) {
        Set<String> selectorIds = new TreeSet<>();
        for (String discoveryHandlerId : discoveryHandlerIds) {
            if (StringUtils.hasLength(discoveryHandlerId)) {
                DiscoveryRelDO rel = discoveryRelMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
                if (Objects.nonNull(rel) && StringUtils.hasLength(rel.getSelectorId())) {
                    selectorIds.add(rel.getSelectorId());
                }
            }
        }
        selectorIds.forEach(selectorMapper::selectByIdForUpdate);
        if (assertSelectorWritesAllowed) {
            selectorIds.forEach(registrationFencingGuard::assertSelectorWriteAllowed);
        }
    }

    private void assertHandlerAddressWriteAllowed(final String discoveryHandlerId, final String url) {
        if (StringUtils.isEmpty(discoveryHandlerId)) {
            return;
        }
        DiscoveryRelDO rel = discoveryRelMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
        if (Objects.nonNull(rel) && StringUtils.hasLength(rel.getSelectorId())) {
            registrationFencingGuard.assertSelectorAddressWriteAllowed(rel.getSelectorId(), url);
        }
    }

    private String canonicalEndpoint(final String endpoint) {
        try {
            return RegistrationFencingAddress.canonicalEndpoint(endpoint);
        } catch (IllegalArgumentException exception) {
            return endpoint;
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<DiscoveryUpstreamDTO> discoveryUpstreamList) {
        if (CollectionUtils.isEmpty(discoveryUpstreamList)) {
            return ConfigImportResult.success();
        }
        discoveryUpstreamList.forEach(upstream -> DiscoveryUpstreamPropsValidator.validate(upstream.getProps()));
        List<String> handlerIds = discoveryUpstreamList.stream().map(DiscoveryUpstreamDTO::getDiscoveryHandlerId)
                .filter(StringUtils::hasText).collect(Collectors.toList());
        lockAndAssertUpstreamWritesAllowed(Collections.emptyList(), handlerIds);
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        Map<String, List<DiscoveryUpstreamDO>> discoveryHandlerUpstreamMap = discoveryUpstreamMapper
                .selectAll()
                .stream()
                .collect(Collectors.groupingBy(DiscoveryUpstreamDO::getDiscoveryHandlerId));
        for (DiscoveryUpstreamDTO discoveryUpstreamDTO : discoveryUpstreamList) {
            String discoveryHandlerId = discoveryUpstreamDTO.getDiscoveryHandlerId();
            String url = discoveryUpstreamDTO.getUrl();
            if (!StringUtils.hasText(discoveryHandlerId)) {
                errorMsgBuilder
                        .append(url)
                        .append(',');
                continue;
            }
            Set<String> existsUpstreamUrlSet = discoveryHandlerUpstreamMap
                    .getOrDefault(discoveryHandlerId, Lists.newArrayList())
                    .stream()
                    .map(DiscoveryUpstreamDO::getUpstreamUrl)
                    .collect(Collectors.toSet());
            if (existsUpstreamUrlSet.contains(url)) {
                errorMsgBuilder
                        .append(url)
                        .append(",");
                continue;
            }
            discoveryUpstreamDTO.setId(null);
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
            discoveryUpstreamMapper.insert(discoveryUpstreamDO);
            successCount++;
        }
        if (StringUtils.hasLength(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail discovery upstream: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<DiscoveryUpstreamDTO> discoveryUpstreamList, final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(discoveryUpstreamList)) {
            return ConfigImportResult.success();
        }
        Map<String, String> discoveryHandlerIdMapping = context.getDiscoveryHandlerIdMapping();
        discoveryUpstreamList.forEach(upstream -> DiscoveryUpstreamPropsValidator.validate(upstream.getProps()));
        List<String> handlerIds = discoveryUpstreamList.stream().map(DiscoveryUpstreamDTO::getDiscoveryHandlerId)
                .map(discoveryHandlerId -> discoveryHandlerIdMapping.getOrDefault(discoveryHandlerId, discoveryHandlerId))
                .filter(StringUtils::hasText).collect(Collectors.toList());
        lockAndAssertUpstreamWritesAllowed(Collections.emptyList(), handlerIds);
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        Map<String, Set<String>> discoveryHandlerUpstreamMap = discoveryUpstreamMapper
                .selectByNamespaceId(namespace)
                .stream()
                .collect(Collectors.groupingBy(DiscoveryUpstreamDO::getDiscoveryHandlerId,
                        Collectors.mapping(DiscoveryUpstreamDO::getUpstreamUrl, Collectors.toSet())));
        for (DiscoveryUpstreamDTO discoveryUpstreamDTO : discoveryUpstreamList) {
            String discoveryHandlerId = discoveryUpstreamDTO.getDiscoveryHandlerId();
            String url = discoveryUpstreamDTO.getUrl();
            String targetHandlerId = discoveryHandlerIdMapping.getOrDefault(discoveryHandlerId, discoveryHandlerId);
            Set<String> existsUpstreamUrlSet = discoveryHandlerUpstreamMap.computeIfAbsent(targetHandlerId, key -> new HashSet<>());
            if (existsUpstreamUrlSet.contains(url) || !StringUtils.hasText(targetHandlerId)) {
                errorMsgBuilder
                        .append(url)
                        .append(',');
                continue;
            }
            discoveryUpstreamDTO.setNamespaceId(namespace);
            discoveryUpstreamDTO.setId(null);
            discoveryUpstreamDTO.setDiscoveryHandlerId(targetHandlerId);
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
            if (discoveryUpstreamMapper.insert(discoveryUpstreamDO) > 0) {
                existsUpstreamUrlSet.add(url);
                successCount++;
            }
        }
        if (StringUtils.hasLength(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail discovery upstream: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    /**
     * Validate the binding before pushing the persisted upstream snapshot to discovery.
     * This method does not undo preceding database writes. Callers own the transaction boundary:
     * a validation failure prevents the push, but writes outside a transaction remain persisted.
     *
     * @param discoveryHandlerId the handler whose upstreams should be published
     */
    private void fetchAll(final String discoveryHandlerId) {
        final List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        Assert.notNull(discoveryHandlerDO, "Discovery handler does not exist: " + discoveryHandlerId);
        ProxySelectorDO proxySelectorDO = proxySelectorMapper.selectByHandlerId(discoveryHandlerId);
        ProxySelectorDTO proxySelectorDTO;
        if (Objects.isNull(proxySelectorDO)) {
            SelectorDO selectorDO = selectorMapper.selectByDiscoveryHandlerId(discoveryHandlerDO.getId());
            Assert.notNull(selectorDO, "Selector binding does not exist for discovery handler: " + discoveryHandlerId);
            PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
            Assert.notNull(pluginDO, "Plugin does not exist for selector: " + selectorDO.getId());
            proxySelectorDTO = new ProxySelectorDTO();
            proxySelectorDTO.setId(selectorDO.getId());
            proxySelectorDTO.setPluginName(pluginDO.getName());
            proxySelectorDTO.setName(selectorDO.getSelectorName());
            proxySelectorDTO.setNamespaceId(selectorDO.getNamespaceId());
        } else {
            proxySelectorDTO = DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO);
        }
        DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
        Assert.notNull(discoveryDO, "Discovery does not exist: " + discoveryHandlerDO.getDiscoveryId());
        List<DiscoveryUpstreamDTO> collect = discoveryUpstreamDOS.stream().map(DiscoveryTransfer.INSTANCE::mapToDTO).collect(Collectors.toList());
        DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType());
        discoveryProcessor.changeUpstream(proxySelectorDTO, collect);
    }

}
