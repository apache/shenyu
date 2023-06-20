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
import org.apache.shenyu.admin.mapper.*;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.entity.*;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ProxySelectorService}.
 */
@Service
public class ProxySelectorServiceImpl implements ProxySelectorService {

    private final ProxySelectorMapper proxySelectorMapper;

    @Autowired
    private DiscoveryMapper discoveryMapper;

    @Autowired
    private DiscoveryRelMapper discoveryRelMapper;

    @Autowired
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Autowired
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    public ProxySelectorServiceImpl(final ProxySelectorMapper proxySelectorMapper) {

        this.proxySelectorMapper = proxySelectorMapper;
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
                    vo.setListenerNode(discoveryHandlerDO.getListenerNode());
                    vo.setHandler(discoveryHandlerDO.getHandler());
                    DiscoveryDO discoveryDO = discoveryMapper.selectById(discoveryHandlerDO.getDiscoveryId());
                    DiscoveryDTO discoveryDTO = new DiscoveryDTO();
                    BeanUtils.copyProperties(discoveryDO, discoveryDTO);
                    vo.setDiscovery(discoveryDTO);
                    List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryRelDO.getDiscoveryHandlerId());
                    List<DiscoveryUpstreamDTO> discoveryUpstreamDTOs = Lists.newArrayList();
                    discoveryUpstreamDOS.forEach(e -> {
                        DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
                        BeanUtils.copyProperties(e, discoveryUpstreamDTO);
                        discoveryUpstreamDTOs.add(discoveryUpstreamDTO);
                    });
                    vo.setDiscoveryUpstreams(discoveryUpstreamDTOs);
                }
                result.add(vo);
            }
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
            addProxySelector(proxySelectorAddDTO);
            return ShenyuResultMessage.CREATE_SUCCESS;
        }
    }

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    @Override
    public String delete(final List<String> ids) {

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
    public Integer addProxySelector(final ProxySelectorAddDTO proxySelectorAddDTO) {

        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        int result = 0;
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorAddDTO);
        String proxySelectorId = proxySelectorDO.getId();
        if (proxySelectorMapper.insert(proxySelectorDO) > 0) {
            result += 1;
            String discoveryId = UUIDUtils.getInstance().generateShortUuid();
            DiscoveryDO discoveryDO = DiscoveryDO.builder()
                    .id(discoveryId)
                    .name(proxySelectorAddDTO.getName())
                    .type(proxySelectorAddDTO.getDiscovery().getDiscoveryType())
                    .serverList(proxySelectorAddDTO.getDiscovery().getServerList())
                    .level("2")
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime)
                    .build();
            if (discoveryMapper.insertSelective(discoveryDO) > 0) {
                // insert discovery handler
                String discoveryHandlerId = UUIDUtils.getInstance().generateShortUuid();
                DiscoveryHandlerDO discoveryHandlerDO = DiscoveryHandlerDO.builder()
                        .id(discoveryHandlerId)
                        .discoveryId(discoveryId)
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .listenerNode(proxySelectorAddDTO.getListenerNode())
                        .handler(proxySelectorAddDTO.getHandler())
                        .props(proxySelectorAddDTO.getProps())
                        .build();
                discoveryHandlerMapper.insertSelective(discoveryHandlerDO);
                DiscoveryRelDO discoveryRelDO = DiscoveryRelDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .pluginName(proxySelectorAddDTO.getName())
                        .discoveryHandlerId(discoveryHandlerId)
                        .proxySelectorId(proxySelectorId)
                        .selectorId("")
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build();
                discoveryRelMapper.insertSelective(discoveryRelDO);
                List<DiscoveryUpstreamDO> upstreamDOList = Lists.newArrayList();
                proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
                    DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                            .id(UUIDUtils.getInstance().generateShortUuid())
                            .discoveryHandlerId(discoveryHandlerId)
                            .protocol(discoveryUpstream.getProtocol())
                            .url(discoveryUpstream.getUrl())
                            .status(discoveryUpstream.getStatus())
                            .props(proxySelectorAddDTO.getProps())
                            .dateCreated(currentTime)
                            .dateUpdated(currentTime)
                            .build();
                    upstreamDOList.add(discoveryUpstreamDO);
                });
                result = discoveryUpstreamMapper.saveBatch(upstreamDOList) + result + 2;
            }
        }
        return result;
    }

    /**
     * update.
     *
     * @param proxySelectorAddDTO proxySelectorAddDTO
     * @return the string
     */
    public String update(final ProxySelectorAddDTO proxySelectorAddDTO) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        // update proxy selector
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorAddDTO);
        proxySelectorMapper.update(proxySelectorDO);
        // DiscoveryRelDO
        DiscoveryRelDO discoveryRelDO = discoveryRelMapper.selectByProxySelectorId(proxySelectorDO.getId());
        String discoveryHandlerId = discoveryRelDO.getDiscoveryHandlerId();
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectById(discoveryHandlerId);
        // update discovery handler
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
        proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
            DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                  .id(discoveryUpstream.getId())
                  .protocol(discoveryUpstream.getProtocol())
                  .url(discoveryUpstream.getUrl())
                  .status(discoveryUpstream.getStatus())
                  .props(discoveryUpstream.getProps())
                  .dateUpdated(currentTime)
                  .build();
            discoveryUpstreamMapper.updateSelective(discoveryUpstreamDO);
        });
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }
}
