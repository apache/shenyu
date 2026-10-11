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
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.discovery.DiscoveryLevel;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ProxySelectorService}.
 */
@Service
public class ProxySelectorServiceImpl implements ProxySelectorService {

    private static final Logger LOG = LoggerFactory.getLogger(ProxySelectorServiceImpl.class);

    // Stay below Oracle's 1000-expression IN limit, including for large requested pages.
    private static final int QUERY_BATCH_SIZE = 500;

    private final ProxySelectorMapper proxySelectorMapper;

    private final DiscoveryMapper discoveryMapper;

    private final DiscoveryRelMapper discoveryRelMapper;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final SelectorMapper selectorMapper;

    private final DiscoveryProcessorHolder discoveryProcessorHolder;

    private final ApplicationEventPublisher eventPublisher;

    public ProxySelectorServiceImpl(final ProxySelectorMapper proxySelectorMapper, final DiscoveryMapper discoveryMapper,
                                    final DiscoveryUpstreamMapper discoveryUpstreamMapper, final DiscoveryHandlerMapper discoveryHandlerMapper,
                                    final DiscoveryRelMapper discoveryRelMapper,
                                    final SelectorMapper selectorMapper,
                                    final DiscoveryProcessorHolder discoveryProcessorHolder, final ApplicationEventPublisher eventPublisher) {

        this.proxySelectorMapper = proxySelectorMapper;
        this.discoveryMapper = discoveryMapper;
        this.discoveryRelMapper = discoveryRelMapper;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.selectorMapper = selectorMapper;
        this.discoveryProcessorHolder = discoveryProcessorHolder;
        this.eventPublisher = eventPublisher;
    }

    /**
     * listByPage.
     *
     * @param query query
     * @return page
     */
    @Override
    @Pageable
    public CommonPager<ProxySelectorVO> listByPage(final ProxySelectorQuery query) {
        List<ProxySelectorVO> result = Lists.newArrayList();
        List<ProxySelectorDO> proxySelectorDOList = proxySelectorMapper.selectByQuery(query);
        if (proxySelectorDOList.isEmpty()) {
            return PageResultUtils.result(query.getPageParameter(), () -> result);
        }
        List<String> selectorIds = proxySelectorDOList.stream().map(ProxySelectorDO::getId).collect(Collectors.toList());
        Map<String, DiscoveryRelDO> relations = queryBatches(selectorIds, discoveryRelMapper::selectByProxySelectorIds).stream()
                .collect(Collectors.toMap(DiscoveryRelDO::getProxySelectorId, relation -> relation));
        List<String> handlerIds = relations.values().stream().map(DiscoveryRelDO::getDiscoveryHandlerId).filter(Objects::nonNull).distinct().collect(Collectors.toList());
        Map<String, DiscoveryHandlerDO> handlers = queryBatches(handlerIds, discoveryHandlerMapper::selectByIds).stream()
                .collect(Collectors.toMap(DiscoveryHandlerDO::getId, handler -> handler));
        List<String> discoveryIds = handlers.values().stream().map(DiscoveryHandlerDO::getDiscoveryId).filter(Objects::nonNull).distinct().collect(Collectors.toList());
        Map<String, DiscoveryDO> discoveries = queryBatches(discoveryIds, discoveryMapper::selectByIds).stream()
                .collect(Collectors.toMap(DiscoveryDO::getId, discovery -> discovery));
        Map<String, List<DiscoveryUpstreamDO>> upstreams = queryBatches(Lists.newArrayList(handlers.keySet()), discoveryUpstreamMapper::selectByDiscoveryHandlerIds).stream()
                .collect(Collectors.groupingBy(DiscoveryUpstreamDO::getDiscoveryHandlerId));
        proxySelectorDOList.forEach(proxySelectorDO -> {
            ProxySelectorVO vo = new ProxySelectorVO();
            vo.setId(proxySelectorDO.getId());
            vo.setName(proxySelectorDO.getName());
            vo.setType(proxySelectorDO.getType());
            vo.setNamespaceId(proxySelectorDO.getNamespaceId());
            vo.setForwardPort(proxySelectorDO.getForwardPort());
            vo.setCreateTime(proxySelectorDO.getDateCreated());
            vo.setUpdateTime(proxySelectorDO.getDateUpdated());
            vo.setProps(proxySelectorDO.getProps());
            DiscoveryRelDO discoveryRelDO = relations.get(proxySelectorDO.getId());
            if (Objects.nonNull(discoveryRelDO)) {
                DiscoveryHandlerDO discoveryHandlerDO = handlers.get(discoveryRelDO.getDiscoveryHandlerId());
                if (Objects.nonNull(discoveryHandlerDO)) {
                    vo.setDiscoveryHandlerId(discoveryHandlerDO.getId());
                    vo.setListenerNode(discoveryHandlerDO.getListenerNode());
                    vo.setHandler(discoveryHandlerDO.getHandler());
                    DiscoveryDO discoveryDO = discoveries.get(discoveryHandlerDO.getDiscoveryId());
                    DiscoveryDTO discoveryDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryDO);
                    vo.setDiscovery(discoveryDTO);
                    List<DiscoveryUpstreamDO> discoveryUpstreamDOList = upstreams.getOrDefault(discoveryRelDO.getDiscoveryHandlerId(), Collections.emptyList());
                    Optional.ofNullable(discoveryUpstreamDOList).ifPresent(list -> {
                        List<DiscoveryUpstreamVO> upstreamVOS = list.stream().map(DiscoveryTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
                        vo.setDiscoveryUpstreams(upstreamVOS);
                    });
                }
            }
            result.add(vo);
        });
        return PageResultUtils.result(query.getPageParameter(), () -> result);
    }

    private <T> List<T> queryBatches(final List<String> ids, final Function<List<String>, List<T>> query) {
        List<T> result = Lists.newArrayList();
        for (List<String> batch : Lists.partition(ids, QUERY_BATCH_SIZE)) {
            result.addAll(query.apply(batch));
        }
        return result;
    }

    /**
     * createOrUpdate.
     *
     * @param proxySelectorAddDTO proxySelectorAddDTO
     * @return the string
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final ProxySelectorAddDTO proxySelectorAddDTO) {
        if (StringUtils.hasLength(proxySelectorAddDTO.getId())) {
            return update(proxySelectorAddDTO);
        } else {
            return create(proxySelectorAddDTO);
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
        for (String proxySelectorId : ids) {
            DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectByProxySelectorId(proxySelectorId);
            if (Objects.nonNull(discoveryHandlerDO)) {
                ProxySelectorDO proxySelectorDO = proxySelectorMapper.selectById(proxySelectorId);
                DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
                DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType());
                discoveryProcessor.removeProxySelector(DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO), DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO));
                if (DiscoveryLevel.SELECTOR.getCode().equals(discoveryDO.getDiscoveryLevel())) {
                    discoveryProcessor.removeDiscovery(discoveryDO);
                    discoveryMapper.delete(discoveryDO.getId(), discoveryDO.getNamespaceId());
                }
                discoveryUpstreamMapper.deleteByDiscoveryHandlerId(discoveryHandlerDO.getId());
                discoveryHandlerMapper.delete(discoveryHandlerDO.getId());
                discoveryRelMapper.deleteByDiscoveryHandlerId(discoveryHandlerDO.getId());
            }
        }
        proxySelectorMapper.deleteByIds(ids);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    /**
     * add proxy selector.
     *
     * @param proxySelectorAddDTO {@link ProxySelectorAddDTO}
     * @return insert data count
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public String create(final ProxySelectorAddDTO proxySelectorAddDTO) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorAddDTO);
        String proxySelectorId = proxySelectorDO.getId();
        if (proxySelectorMapper.insert(proxySelectorDO) > 0) {
            DiscoveryProcessor discoveryProcessor;
            DiscoveryDO discoveryDO;
            String discoveryId;
            boolean fillDiscovery;
            if (StringUtils.hasLength(proxySelectorAddDTO.getDiscovery().getId())) {
                discoveryDO = discoveryMapper.selectById(proxySelectorAddDTO.getDiscovery().getId());
                Assert.notNull(discoveryDO, "Discovery does not exist: " + proxySelectorAddDTO.getDiscovery().getId());
                Assert.isTrue(Objects.equals(discoveryDO.getNamespaceId(), proxySelectorAddDTO.getNamespaceId()),
                        "Discovery does not belong to namespace: " + proxySelectorAddDTO.getNamespaceId());
                discoveryId = proxySelectorAddDTO.getDiscovery().getId();
                fillDiscovery = true;
                // the stored discovery is the source of truth for the processor type; the
                // payload could otherwise declare a different type than the referenced row
                discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType());
            } else {
                discoveryId = UUIDUtils.getInstance().generateShortUuid();
                discoveryDO = buildDiscovery(proxySelectorAddDTO, currentTime, discoveryId);
                fillDiscovery = discoveryMapper.insertSelective(discoveryDO) > 0;
                discoveryProcessor = discoveryProcessorHolder.chooseProcessor(proxySelectorAddDTO.getDiscovery().getDiscoveryType());
                discoveryProcessor.createDiscovery(discoveryDO);
            }
            if (fillDiscovery) {
                // insert discovery handler
                String discoveryHandlerId = UUIDUtils.getInstance().generateShortUuid();
                DiscoveryHandlerDO discoveryHandlerDO = DiscoveryHandlerDO.builder()
                        .id(discoveryHandlerId)
                        .discoveryId(discoveryId)
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .listenerNode(proxySelectorAddDTO.getListenerNode())
                        .handler(Objects.isNull(proxySelectorAddDTO.getHandler()) ? "" : proxySelectorAddDTO.getHandler())
                        .props(proxySelectorAddDTO.getProps())
                        .build();
                discoveryHandlerMapper.insertSelective(discoveryHandlerDO);
                DiscoveryRelDO discoveryRelDO = DiscoveryRelDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .pluginName(proxySelectorAddDTO.getPluginName())
                        .discoveryHandlerId(discoveryHandlerId)
                        .proxySelectorId(proxySelectorId)
                        .selectorId("")
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build();
                discoveryRelMapper.insertSelective(discoveryRelDO);
                DiscoveryHandlerDTO discoveryHandlerDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO);
                ProxySelectorDTO proxySelectorDTO = DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO);
                proxySelectorDTO.setId(proxySelectorId);
                discoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO);
                addUpstreamList(proxySelectorAddDTO, currentTime, discoveryProcessor, discoveryHandlerId, proxySelectorDTO);
            }
        }
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    private void addUpstreamList(final ProxySelectorAddDTO proxySelectorAddDTO, final Timestamp currentTime, final DiscoveryProcessor discoveryProcessor,
                                 final String discoveryHandlerId, final ProxySelectorDTO proxySelectorDTO) {
        List<DiscoveryUpstreamDO> upstreamDOList = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(proxySelectorAddDTO.getDiscoveryUpstreams())) {
            proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
                DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .discoveryHandlerId(discoveryHandlerId)
                        .namespaceId(proxySelectorAddDTO.getNamespaceId())
                        .protocol(discoveryUpstream.getProtocol())
                        .url(discoveryUpstream.getUrl())
                        .status(discoveryUpstream.getStatus())
                        .weight(discoveryUpstream.getWeight())
                        .props(Optional.ofNullable(discoveryUpstream.getProps()).orElse("{}"))
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build();
                upstreamDOList.add(discoveryUpstreamDO);
            });
            discoveryUpstreamMapper.saveBatch(upstreamDOList);
            List<DiscoveryUpstreamDTO> collect = upstreamDOList.stream().map(DiscoveryTransfer.INSTANCE::mapToDTO).collect(Collectors.toList());
            discoveryProcessor.changeUpstream(proxySelectorDTO, collect);
        }
    }

    @NotNull
    private static DiscoveryDO buildDiscovery(final ProxySelectorAddDTO proxySelectorAddDTO, final Timestamp currentTime, final String discoveryId) {
        return DiscoveryDO.builder()
                .id(discoveryId)
                .discoveryName(proxySelectorAddDTO.getName())
                .discoveryType(proxySelectorAddDTO.getDiscovery().getDiscoveryType())
                .serverList(proxySelectorAddDTO.getDiscovery().getServerList())
                .pluginName(proxySelectorAddDTO.getPluginName())
                .namespaceId(proxySelectorAddDTO.getNamespaceId())
                .discoveryLevel(DiscoveryLevel.SELECTOR.getCode())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .props(proxySelectorAddDTO.getDiscovery().getProps())
                .build();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String bindingDiscoveryHandler(final ProxySelectorAddDTO proxySelectorAddDTO) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        String selectorId = proxySelectorAddDTO.getSelectorId();
        final ProxySelectorAddDTO.Discovery discovery = proxySelectorAddDTO.getDiscovery();
        Assert.notNull(discovery, "Discovery configuration is required for selector: " + selectorId);
        DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discovery.getDiscoveryType());
        String discoveryId = discovery.getId();
        if (!StringUtils.hasLength(discoveryId)) {
            discoveryId = UUIDUtils.getInstance().generateShortUuid();
            DiscoveryDO discoveryDO = buildDiscovery(proxySelectorAddDTO, currentTime, discoveryId);
            discoveryMapper.insertSelective(discoveryDO);
            discoveryProcessor.createDiscovery(discoveryDO);
        }
        String discoveryHandlerId = UUIDUtils.getInstance().generateShortUuid();
        DiscoveryHandlerDO discoveryHandlerDO = DiscoveryHandlerDO.builder()
                .id(discoveryHandlerId)
                .discoveryId(discoveryId)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .listenerNode(proxySelectorAddDTO.getListenerNode())
                .handler(Objects.isNull(proxySelectorAddDTO.getHandler()) ? "" : proxySelectorAddDTO.getHandler())
                .props(proxySelectorAddDTO.getProps())
                .build();
        discoveryHandlerMapper.insertSelective(discoveryHandlerDO);
        DiscoveryRelDO discoveryRelDO = DiscoveryRelDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .pluginName(proxySelectorAddDTO.getPluginName())
                .discoveryHandlerId(discoveryHandlerId)
                .selectorId(selectorId)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        discoveryRelMapper.insertSelective(discoveryRelDO);
        ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        proxySelectorDTO.setPluginName(proxySelectorAddDTO.getPluginName());
        proxySelectorDTO.setName(proxySelectorAddDTO.getName());
        proxySelectorDTO.setId(selectorId);
        proxySelectorDTO.setNamespaceId(proxySelectorAddDTO.getNamespaceId());
        DiscoveryHandlerDTO discoveryHandlerDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO);
        discoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO);
        addUpstreamList(proxySelectorAddDTO, currentTime, discoveryProcessor, discoveryHandlerId, proxySelectorDTO);
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    /**
     * update.
     *
     * @param proxySelectorAddDTO proxySelectorAddDTO
     * @return the string
     */
    @Transactional(rollbackFor = Exception.class)
    public String update(final ProxySelectorAddDTO proxySelectorAddDTO) {
        ProxySelectorAddDTO.Discovery discovery = proxySelectorAddDTO.getDiscovery();
        Assert.notNull(discovery, "Discovery configuration is required");
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorAddDTO);
        DiscoveryRelDO discoveryRelDO = discoveryRelMapper.selectByProxySelectorId(proxySelectorDO.getId());
        Assert.notNull(discoveryRelDO, "Discovery binding does not exist for proxy selector: " + proxySelectorDO.getId());
        String discoveryHandlerId = discoveryRelDO.getDiscoveryHandlerId();
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        Assert.notNull(discoveryHandlerDO, "Discovery handler does not exist: " + discoveryHandlerId);
        DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
        Assert.notNull(discoveryDO, "Discovery does not exist: " + discoveryHandlerDO.getDiscoveryId());
        Assert.isTrue(Objects.equals(discoveryDO.getNamespaceId(), proxySelectorAddDTO.getNamespaceId()),
                "Discovery does not belong to namespace: " + proxySelectorAddDTO.getNamespaceId());
        // Validate all related records before performing any update.
        proxySelectorMapper.update(proxySelectorDO);
        // update discovery handler
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        discoveryHandlerDO.setHandler(proxySelectorAddDTO.getHandler());
        discoveryHandlerDO.setListenerNode(proxySelectorAddDTO.getListenerNode());
        discoveryHandlerDO.setProps(proxySelectorAddDTO.getProps());
        discoveryHandlerDO.setDateUpdated(currentTime);
        discoveryHandlerMapper.updateSelective(discoveryHandlerDO);
        // update discovery
        discoveryDO.setServerList(discovery.getServerList());
        discoveryDO.setDateUpdated(currentTime);
        discoveryDO.setProps(discovery.getProps());
        discoveryMapper.updateSelective(discoveryDO);
        // update discovery upstream list
        if (!CollectionUtils.isEmpty(proxySelectorAddDTO.getDiscoveryUpstreams())) {
            int result = discoveryUpstreamMapper.deleteByDiscoveryHandlerId(discoveryHandlerId);
            LOG.info("delete discovery upstreams, count is: {}", result);
            proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
                DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .discoveryHandlerId(discoveryHandlerId)
                        .namespaceId(discoveryDO.getNamespaceId())
                        .protocol(discoveryUpstream.getProtocol())
                        .url(discoveryUpstream.getUrl())
                        .status(discoveryUpstream.getStatus())
                        .weight(discoveryUpstream.getWeight())
                        .props(discoveryUpstream.getProps())
                        .dateCreated(Optional.ofNullable(discoveryUpstream.getStartupTime()).map(t -> new Timestamp(Long.parseLong(t))).orElse(currentTime))
                        .dateUpdated(Optional.ofNullable(discoveryUpstream.getStartupTime()).map(t -> new Timestamp(Long.parseLong(t))).orElse(currentTime))
                        .build();
                discoveryUpstreamMapper.insert(discoveryUpstreamDO);
            });
            LOG.info("insert discovery upstreams, count is: {}", proxySelectorAddDTO.getDiscoveryUpstreams().size());
        }
        List<DiscoveryUpstreamDTO> fetchAll = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDO.getId()).stream()
                .map(DiscoveryTransfer.INSTANCE::mapToDTO).collect(Collectors.toList());
        DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType());
        discoveryProcessor.changeUpstream(DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO), fetchAll);
        DataChangedEvent event = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.UPDATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO))));
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCompletion(final int status) {
                    // Completion runs after synchronization is cleared, so the cluster dispatcher does not defer the event again.
                    if (status == STATUS_COMMITTED) {
                        eventPublisher.publishEvent(event);
                    }
                }
            });
        } else {
            eventPublisher.publishEvent(event);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    @Override
    public void fetchData(final String discoveryHandlerId) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        if (Objects.isNull(discoveryHandlerDO)) {
            return;
        }
        DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
        if (Objects.isNull(discoveryDO)) {
            return;
        }
        ProxySelectorDO proxySelectorDO = proxySelectorMapper.selectByHandlerId(discoveryHandlerId);
        DiscoveryHandlerDTO discoveryHandlerDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO);
        if (Objects.nonNull(proxySelectorDO)) {
            discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType()).fetchAll(discoveryHandlerDTO, DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO));
        }
        SelectorDO selectorDO = selectorMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
        if (Objects.nonNull(selectorDO)) {
            ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
            proxySelectorDTO.setPluginName(discoveryDO.getPluginName());
            proxySelectorDTO.setName(selectorDO.getSelectorName());
            proxySelectorDTO.setId(selectorDO.getId());
            proxySelectorDTO.setNamespaceId(selectorDO.getNamespaceId());
            discoveryProcessorHolder.chooseProcessor(discoveryDO.getDiscoveryType()).fetchAll(discoveryHandlerDTO, proxySelectorDTO);
        }
    }

    @Override
    public List<ProxySelectorData> listAll() {
        return proxySelectorMapper.selectAll().stream()
                .map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
    }
    
    @Override
    public List<ProxySelectorData> listAllByNamespaceId(final String namespaceId) {
        return proxySelectorMapper.selectByNamespaceId(namespaceId).stream()
                .map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
    }
    
    @Override
    public List<ProxySelectorVO> listAllData() {
        List<ProxySelectorVO> result = Lists.newArrayList();
        proxySelectorMapper.selectAll().forEach(proxySelectorDO -> {
            ProxySelectorVO vo = new ProxySelectorVO();
            vo.setId(proxySelectorDO.getId());
            vo.setName(proxySelectorDO.getName());
            vo.setType(proxySelectorDO.getType());
            vo.setForwardPort(proxySelectorDO.getForwardPort());
            vo.setCreateTime(proxySelectorDO.getDateCreated());
            vo.setUpdateTime(proxySelectorDO.getDateUpdated());
            vo.setProps(proxySelectorDO.getProps());
            DiscoveryRelDO discoveryRelDO = discoveryRelMapper.selectByProxySelectorId(proxySelectorDO.getId());
            if (Objects.nonNull(discoveryRelDO)) {
                DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryRelDO.getDiscoveryHandlerId());
                if (Objects.nonNull(discoveryHandlerDO)) {
                    vo.setDiscoveryHandlerId(discoveryHandlerDO.getId());
                    vo.setListenerNode(discoveryHandlerDO.getListenerNode());
                    vo.setHandler(discoveryHandlerDO.getHandler());
                    DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
                    DiscoveryDTO discoveryDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryDO);
                    vo.setDiscovery(discoveryDTO);
                    List<DiscoveryUpstreamDO> discoveryUpstreamDOList = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryRelDO.getDiscoveryHandlerId());
                    Optional.ofNullable(discoveryUpstreamDOList).ifPresent(list -> {
                        List<DiscoveryUpstreamVO> upstreamVOS = list.stream().map(DiscoveryTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
                        vo.setDiscoveryUpstreams(upstreamVOS);
                    });
                }
            }
            result.add(vo);
        });
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<ProxySelectorData> proxySelectorList) {
        if (CollectionUtils.isEmpty(proxySelectorList)) {
            return ConfigImportResult.success();
        }
        Map<String, List<ProxySelectorDO>> pluginProxySelectorMap = proxySelectorMapper
                .selectAll()
                .stream()
                .collect(Collectors.groupingBy(ProxySelectorDO::getPluginName));
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        for (ProxySelectorData selectorData : proxySelectorList) {
            String pluginName = selectorData.getPluginName();
            String proxySelectorName = selectorData.getName();
            Set<String> existProxySelectorNameSet = pluginProxySelectorMap
                    .getOrDefault(pluginName, Lists.newArrayList())
                    .stream()
                    .map(ProxySelectorDO::getName)
                    .collect(Collectors.toSet());

            if (existProxySelectorNameSet.contains(proxySelectorName)) {
                errorMsgBuilder
                        .append(proxySelectorName)
                        .append(",");
                continue;
            }
            ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(selectorData);
            if (proxySelectorMapper.insert(proxySelectorDO) > 0) {
                successCount++;
            }
        }
        if (StringUtils.hasLength(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail proxy selector: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<ProxySelectorData> proxySelectorList,
                                         final ConfigsImportContext context) {
        if (CollectionUtils.isEmpty(proxySelectorList)) {
            return ConfigImportResult.success();
        }
        Map<String, String> proxySelectorIdMapping = new HashMap<>();
        Map<String, List<ProxySelectorDO>> pluginProxySelectorMap = proxySelectorMapper
                .selectByNamespaceId(namespace)
                .stream()
                .collect(Collectors.groupingBy(ProxySelectorDO::getPluginName));
        int successCount = 0;
        StringBuilder errorMsgBuilder = new StringBuilder();
        for (ProxySelectorData selectorData : proxySelectorList) {
            String pluginName = selectorData.getPluginName();
            String proxySelectorName = selectorData.getName();
            Map<String, String> existProxySelectorNameSet = pluginProxySelectorMap
                    .getOrDefault(pluginName, Lists.newArrayList())
                    .stream()
                    .collect(Collectors.toMap(ProxySelectorDO::getName, ProxySelectorDO::getId));
            
            if (existProxySelectorNameSet.containsKey(proxySelectorName)) {
                errorMsgBuilder
                        .append(proxySelectorName)
                        .append(",");
                proxySelectorIdMapping.put(selectorData.getId(), existProxySelectorNameSet.get(proxySelectorName));
                continue;
            }
            String oldProxySelectorId = selectorData.getId();
            String newProxySelectorId = UUIDUtils.getInstance().generateShortUuid();
            ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(selectorData);
            proxySelectorDO.setId(newProxySelectorId);
            proxySelectorDO.setNamespaceId(namespace);
            if (proxySelectorMapper.insert(proxySelectorDO) > 0) {
                proxySelectorIdMapping.put(oldProxySelectorId, newProxySelectorId);
                successCount++;
            }
        }
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCommit() {
                    context.getProxySelectorIdMapping().putAll(proxySelectorIdMapping);
                }
            });
        } else {
            context.getProxySelectorIdMapping().putAll(proxySelectorIdMapping);
        }
        if (StringUtils.hasLength(errorMsgBuilder)) {
            errorMsgBuilder.setLength(errorMsgBuilder.length() - 1);
            return ConfigImportResult
                    .fail(successCount, "import fail proxy selector: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
}
