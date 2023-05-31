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
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorAddDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;

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

        return PageResultUtils.result(query.getPageParameter(), () -> proxySelectorMapper.selectByQuery(query)
                .stream()
                .map(ProxySelectorVO::buildProxySelectorVO)
                .collect(Collectors.toList()));
    }

    /**
     * createOrUpdate.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    @Override
    public String createOrUpdate(final ProxySelectorDTO proxySelectorDTO) {

        return StringUtils.hasLength(proxySelectorDTO.getId()) ? update(proxySelectorDTO) : create(proxySelectorDTO);
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
    @Transactional(rollbackFor = Exception.class)
    public Integer addProxySelector(final ProxySelectorAddDTO proxySelectorAddDTO) {

        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        int result = 0;
        String proxySelectorId = UUIDUtils.getInstance().generateShortUuid();
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.builder()
                .id(proxySelectorId)
                .name(proxySelectorAddDTO.getName())
                .pluginName(proxySelectorAddDTO.getPluginName())
                .forwardPort(proxySelectorAddDTO.getForwardPort())
                .type(proxySelectorAddDTO.getType())
                .props(proxySelectorAddDTO.getProps())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        if (proxySelectorMapper.insert(proxySelectorDO) > 0) {
            result += 1;
            String discoveryId = UUIDUtils.getInstance().generateShortUuid();
            DiscoveryDO discoveryDO = DiscoveryDO.builder()
                    .id(discoveryId)
                    .name(proxySelectorAddDTO.getName())
                    .pluginName(proxySelectorAddDTO.getDiscovery().getHandler())
                    .type(proxySelectorAddDTO.getDiscovery().getDiscoveryType())
                    .serviceList(proxySelectorAddDTO.getDiscovery().getServiceList())
                    .level(proxySelectorAddDTO.getDiscovery().getListenerNode())
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime)
                    .build();
            if (discoveryMapper.insertSelective(discoveryDO) > 0) {
                result += 1;
                DiscoveryRelDO discoveryRelDO = DiscoveryRelDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .discoveryHandlerId(discoveryId)
                        .proxySelectorId(proxySelectorId)
                        .selectorId("")
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build();
                discoveryRelMapper.insertSelective(discoveryRelDO);
            }
            List<DiscoveryUpstreamDO> upstreamDOList = Lists.newArrayList();
            proxySelectorAddDTO.getDiscoveryUpstreams().forEach(discoveryUpstream -> {
                DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .discoveryHandlerId(discoveryId)
                        .protocol(discoveryUpstream.getProtocol())
                        .url(discoveryUpstream.getUrl())
                        .status(discoveryUpstream.getStatus())
                        .weight(discoveryUpstream.getWeight())
                        .props(proxySelectorAddDTO.getProps())
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build();
                upstreamDOList.add(discoveryUpstreamDO);
            });
            result = discoveryUpstreamMapper.saveBatch(upstreamDOList) + result;
        }

        return result;
    }

    /**
     * create.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    private String create(final ProxySelectorDTO proxySelectorDTO) {

        Assert.isNull(proxySelectorMapper.nameExisted(proxySelectorDTO.getName()),
                AdminConstants.PROXY_SELECTOR_NAME_IS_EXIST);
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorDTO);
        proxySelectorMapper.insert(proxySelectorDO);
        return ShenyuResultMessage.CREATE_SUCCESS;

    }

    /**
     * update.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    private String update(final ProxySelectorDTO proxySelectorDTO) {

        Assert.isNull(proxySelectorMapper.nameExisted(proxySelectorDTO.getName()),
                AdminConstants.PROXY_SELECTOR_NAME_IS_EXIST);
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorDTO);
        proxySelectorMapper.update(proxySelectorDO);
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }
}
