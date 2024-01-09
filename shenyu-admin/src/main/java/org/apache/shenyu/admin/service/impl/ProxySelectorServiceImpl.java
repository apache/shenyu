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
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ProxySelectorService}.
 */
@Service
public class ProxySelectorServiceImpl implements ProxySelectorService {

    private static final Logger LOG = LoggerFactory.getLogger(ProxySelectorServiceImpl.class);

    private final ProxySelectorMapper proxySelectorMapper;

    private final DiscoveryMapper discoveryMapper;

    private final DiscoveryRelMapper discoveryRelMapper;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final SelectorMapper selectorMapper;

    private final DiscoveryProcessorHolder discoveryProcessorHolder;

    public ProxySelectorServiceImpl(final ProxySelectorMapper proxySelectorMapper, final DiscoveryMapper discoveryMapper,
                                    final DiscoveryUpstreamMapper discoveryUpstreamMapper, final DiscoveryHandlerMapper discoveryHandlerMapper,
                                    final DiscoveryRelMapper discoveryRelMapper,
                                    final SelectorMapper selectorMapper,
                                    final DiscoveryProcessorHolder discoveryProcessorHolder) {

        this.proxySelectorMapper = proxySelectorMapper;
        this.discoveryMapper = discoveryMapper;
        this.discoveryRelMapper = discoveryRelMapper;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.selectorMapper = selectorMapper;
        this.discoveryProcessorHolder = discoveryProcessorHolder;
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
        proxySelectorDOList.forEach(proxySelectorDO -> {
            ProxySelectorVO vo = new ProxySelectorVO();
            vo.setId(proxySelectorDO.getId());
            vo.setName(proxySelectorDO.getName());
            vo.setType(proxySelectorDO.getType());
            vo.setForwardPort(proxySelectorDO.getForwardPort());
            vo.setCreateTime(proxySelectorDO.getDateCreated());
            vo.setUpdateTime(proxySelectorDO.getDateUpdated());
            vo.setProps(proxySelectorDO.getProps());
            DiscoveryRelDO discoveryRelDO = discoveryRelMapper.selectByProxySelectorId(proxySelectorDO.getId());
            if (!Objects.isNull(discoveryRelDO)) {
                DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryRelDO.getDiscoveryHandlerId());
                if (!Objects.isNull(discoveryHandlerDO)) {
                    vo.setDiscoveryHandlerId(discoveryHandlerDO.getId());
                    vo.setListenerNode(discoveryHandlerDO.getListenerNode());
                    vo.setHandler(discoveryHandlerDO.getHandler());
                    DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
                    DiscoveryDTO discoveryDTO = new DiscoveryDTO();
                    BeanUtils.copyProperties(discoveryDO, discoveryDTO);
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
        return PageResultUtils.result(query.getPageParameter(), () -> result);
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
                DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getType());
                discoveryProcessor.removeProxySelector(DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO), DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO));
                if (DiscoveryLevel.SELECTOR.getCode().equals(discoveryDO.getLevel())) {
                    discoveryProcessor.removeDiscovery(discoveryDO);
                    discoveryMapper.delete(discoveryDO.getId());
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
            DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(proxySelectorAddDTO.getDiscovery().getDiscoveryType());
            DiscoveryDO discoveryDO;
            String discoveryId;
            boolean fillDiscovery;
            if (StringUtils.hasLength(proxySelectorAddDTO.getDiscovery().getId())) {
                discoveryDO = discoveryMapper.selectById(proxySelectorAddDTO.getDiscovery().getId());
                discoveryId = proxySelectorAddDTO.getDiscovery().getId();
                fillDiscovery = Objects.nonNull(discoveryDO);
            } else {
                discoveryId = UUIDUtils.getInstance().generateShortUuid();
                discoveryDO = buildDiscovery(proxySelectorAddDTO, currentTime, discoveryId);
                fillDiscovery = discoveryMapper.insertSelective(discoveryDO) > 0;
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
                        .handler(proxySelectorAddDTO.getHandler() == null ? "" : proxySelectorAddDTO.getHandler())
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
                .name(proxySelectorAddDTO.getName())
                .type(proxySelectorAddDTO.getDiscovery().getDiscoveryType())
                .serverList(proxySelectorAddDTO.getDiscovery().getServerList())
                .pluginName(proxySelectorAddDTO.getPluginName())
                .level(DiscoveryLevel.SELECTOR.getCode())
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
        DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(proxySelectorAddDTO.getDiscovery().getDiscoveryType());
        ProxySelectorAddDTO.Discovery discovery = proxySelectorAddDTO.getDiscovery();
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
                .handler(proxySelectorAddDTO.getHandler() == null ? "" : proxySelectorAddDTO.getHandler())
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
        // update proxy selector
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorAddDTO);
        proxySelectorMapper.update(proxySelectorDO);
        // DiscoveryRelDO
        DiscoveryRelDO discoveryRelDO = discoveryRelMapper.selectByProxySelectorId(proxySelectorDO.getId());
        String discoveryHandlerId = discoveryRelDO.getDiscoveryHandlerId();
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        // update discovery handler
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        discoveryHandlerDO.setHandler(proxySelectorAddDTO.getHandler());
        discoveryHandlerDO.setListenerNode(proxySelectorAddDTO.getListenerNode());
        discoveryHandlerDO.setProps(proxySelectorAddDTO.getProps());
        discoveryHandlerDO.setDateUpdated(currentTime);
        discoveryHandlerMapper.updateSelective(discoveryHandlerDO);
        // update discovery
        DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
        ProxySelectorAddDTO.Discovery discovery = proxySelectorAddDTO.getDiscovery();
        discoveryDO.setServerList(discovery.getServerList());
        discoveryDO.setDateUpdated(currentTime);
        discoveryDO.setProps(discovery.getProps());
        discoveryMapper.updateSelective(discoveryDO);
        // update discovery upstream list
        int result = discoveryUpstreamMapper.deleteByDiscoveryHandlerId(discoveryHandlerId);
        LOG.info("delete discovery upstreams, count is: {}", result);
        proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                    .id(UUIDUtils.getInstance().generateShortUuid())
                    .discoveryHandlerId(discoveryHandlerId)
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
        List<DiscoveryUpstreamDTO> fetchAll = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerDO.getId()).stream()
                .map(DiscoveryTransfer.INSTANCE::mapToDTO).collect(Collectors.toList());
        DiscoveryProcessor discoveryProcessor = discoveryProcessorHolder.chooseProcessor(discoveryDO.getType());
        discoveryProcessor.changeUpstream(DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO), fetchAll);
        LOG.info("insert discovery upstreams, count is: {}", proxySelectorAddDTO.getDiscoveryUpstreams().size());
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    @Override
    public void fetchData(final String discoveryHandlerId) {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
        ProxySelectorDO proxySelectorDO = proxySelectorMapper.selectByHandlerId(discoveryHandlerId);
        DiscoveryHandlerDTO discoveryHandlerDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryHandlerDO);
        if (Objects.nonNull(proxySelectorDO)) {
            discoveryProcessorHolder.chooseProcessor(discoveryDO.getType()).fetchAll(discoveryHandlerDTO, DiscoveryTransfer.INSTANCE.mapToDTO(proxySelectorDO));
        }
        SelectorDO selectorDO = selectorMapper.selectByDiscoveryHandlerId(discoveryHandlerId);
        if (Objects.nonNull(selectorDO)) {
            ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
            proxySelectorDTO.setPluginName(discoveryDO.getPluginName());
            proxySelectorDTO.setName(selectorDO.getName());
            proxySelectorDTO.setId(selectorDO.getId());
            discoveryProcessorHolder.chooseProcessor(discoveryDO.getType()).fetchAll(discoveryHandlerDTO, proxySelectorDTO);
        }
    }

    @Override
    public List<ProxySelectorData> listAll() {
        return proxySelectorMapper.selectAll().stream()
                .map(DiscoveryTransfer.INSTANCE::mapToData).collect(Collectors.toList());
    }
}
