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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.namespace.NamespaceCreatedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.service.publish.NamespaceEventPublisher;
import org.apache.shenyu.admin.transfer.NamespaceTransfer;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.NamespaceIDUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class NamespaceServiceImpl implements NamespaceService {

    private final NamespaceMapper namespaceMapper;

    private final NamespaceUserService namespaceUserService;

    private final NamespaceEventPublisher namespaceEventPublisher;

    private final SelectorMapper selectorMapper;

    private final RuleMapper ruleMapper;

    private final AuthPathMapper authPathMapper;

    private final MetaDataMapper metaDataMapper;

    private final DiscoveryMapper discoveryMapper;

    private final NamespacePluginRelMapper namespacePluginRelMapper;


    public NamespaceServiceImpl(final NamespaceMapper namespaceMapper,
                                final NamespaceUserService namespaceUserService,
                                final NamespaceEventPublisher namespaceEventPublisher,
                                final NamespacePluginRelMapper namespacePluginRelMapper,
                                final SelectorMapper selectorMapper,
                                final RuleMapper ruleMapper,
                                final AuthPathMapper authPathMapper,
                                final MetaDataMapper metaDataMapper,
                                final DiscoveryMapper discoveryMapper) {
        this.namespaceMapper = namespaceMapper;
        this.namespaceUserService = namespaceUserService;
        this.namespaceEventPublisher = namespaceEventPublisher;
        this.namespacePluginRelMapper = namespacePluginRelMapper;
        this.selectorMapper = selectorMapper;
        this.ruleMapper = ruleMapper;
        this.authPathMapper = authPathMapper;
        this.metaDataMapper = metaDataMapper;
        this.discoveryMapper = discoveryMapper;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public NamespaceVO createOrUpdate(final NamespaceDTO namespaceDTO) {
        return StringUtils.isBlank(namespaceDTO.getId())
                ? this.create(namespaceDTO) : this.update(namespaceDTO);
    }

    @Override
    public CommonPager<NamespaceVO> listByPage(final NamespaceQuery namespaceQuery) {
        List<String> namespaceIds;
        if (SessionUtil.isAdmin()) {
            List<NamespaceDO> allList = namespaceMapper.selectAll();
            namespaceIds = allList.stream().map(NamespaceDO::getNamespaceId).toList();
        } else {
            namespaceIds = namespaceUserService.listNamespaceIdByUserId(SessionUtil.visitorId());
        }
        if (CollectionUtils.isEmpty(namespaceIds)) {
            return new CommonPager<>();
        }
        namespaceQuery.setNamespaceIds(namespaceIds);
        return PageResultUtils.result(namespaceQuery.getPageParameter(), () -> namespaceMapper.countByQuery(namespaceQuery), () -> namespaceMapper.selectByQuery(namespaceQuery)
                .stream()
                .map(NamespaceTransfer.INSTANCE::mapToVo)
                .collect(Collectors.toList()));
    }

    @Override
    public String delete(final List<String> ids) {
        if (ids.contains(Constants.DEFAULT_NAMESPACE_PRIMARY_KEY)) {
            throw new ShenyuAdminException(AdminConstants.SYS_DEFAULT_NAMESPACE_ID_DELETE);
        }
        List<String> namespaceIdList = namespaceMapper.selectByIds(ids).stream().map(NamespaceDO::getNamespaceId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(namespaceIdList)) {
            throw new ShenyuAdminException(AdminConstants.SYS_NAMESPACE_ID_NOT_EXIST);
        }
        List<NamespacePluginVO> namespacePluginVOS = namespacePluginRelMapper.selectAllByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(namespacePluginVOS)) {
            throw new ShenyuAdminException("Plugins exist under those namespace!");
        }
        List<SelectorDO> selectorDOS = selectorMapper.selectAllByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(selectorDOS)) {
            throw new ShenyuAdminException("selector exist under those namespace!");
        }
        List<RuleDO> ruleDOList = ruleMapper.selectAllByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(ruleDOList)) {
            throw new ShenyuAdminException("rule exist under those namespace!");
        }
        List<MetaDataDO> metaDataDOList = metaDataMapper.findAllByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(metaDataDOList)) {
            throw new ShenyuAdminException("metaData exist under those namespace!");
        }
        List<AuthPathDO> authPathDOList = authPathMapper.findByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(authPathDOList)) {
            throw new ShenyuAdminException("authPath exist under those namespace!");
        }
        List<DiscoveryDO> discoveryDOList = discoveryMapper.selectAllByNamespaceIds(namespaceIdList);
        if (CollectionUtils.isNotEmpty(discoveryDOList)) {
            throw new ShenyuAdminException("discovery exist under those namespace!");
        }
        namespaceMapper.deleteByIds(ids);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    @Override
    public NamespaceVO findById(final String id) {
        return NamespaceTransfer.INSTANCE.mapToVo(namespaceMapper.selectById(id));
    }

    @Override
    public NamespaceVO findByNamespaceId(final String namespaceId) {
        return NamespaceTransfer.INSTANCE.mapToVo(namespaceMapper.selectByNamespaceId(namespaceId));
    }

    @Override
    public List<NamespaceVO> list(final String name) {
        
        if (SessionUtil.isAdmin()) {
            List<NamespaceDO> allList = namespaceMapper.selectAll();
            return allList.stream().map(NamespaceTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
        }
        
        List<String> namespaceIds = namespaceUserService.listNamespaceIdByUserId(SessionUtil.visitorId());

        if (CollectionUtils.isEmpty(namespaceIds)) {
            return Lists.newArrayList();
        }

        List<NamespaceDO> namespaceDOS = namespaceMapper.selectByNamespaceIdsAndName(namespaceIds, name);
        if (CollectionUtils.isEmpty(namespaceDOS)) {
            return Lists.newArrayList();
        }
        return namespaceDOS.stream().map(NamespaceTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
    }

    @Override
    public List<NamespaceVO> listAll() {
        List<NamespaceDO> namespaceDOS = namespaceMapper.selectAll();
        return namespaceDOS.stream().map(NamespaceTransfer.INSTANCE::mapToVo).collect(Collectors.toList());

    }

    private NamespaceVO create(final NamespaceDTO namespaceDTO) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        String namespaceId = StringUtils.defaultString(namespaceDTO.getNamespaceId(), NamespaceIDUtils.getInstance().generateNamespaceID());
        NamespaceDO namespaceDO = NamespaceDO.builder()
                .id(id)
                .namespaceId(namespaceId)
                .name(namespaceDTO.getName())
                .description(namespaceDTO.getDescription())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        namespaceMapper.insert(namespaceDO);

        namespaceEventPublisher.publish(new NamespaceCreatedEvent(namespaceDO, SessionUtil.visitorId()));

        return NamespaceTransfer.INSTANCE.mapToVo(namespaceDO);
    }

    private NamespaceVO update(final NamespaceDTO namespaceDTO) {
        if (Objects.isNull(namespaceDTO) || Objects.isNull(namespaceDTO.getNamespaceId())) {
            throw new ShenyuAdminException("namespace is not exist");
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        NamespaceDO namespaceDO = NamespaceDO.builder()
                .namespaceId(namespaceDTO.getNamespaceId())
                .name(namespaceDTO.getName())
                .description(namespaceDTO.getDescription())
                .dateUpdated(currentTime)
                .build();
        return namespaceMapper.updateSelective(namespaceDO) > 0
                ? NamespaceTransfer.INSTANCE.mapToVo(namespaceDO) : null;
    }
}
