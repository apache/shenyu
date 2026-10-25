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

import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.InstanceInfoMapper;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.utils.InstanceInfoUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Objects;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.InstanceInfoService}.
 */
@Service
public class InstanceInfoServiceImpl implements InstanceInfoService {

    private static final Logger LOG = LoggerFactory.getLogger(InstanceInfoServiceImpl.class);

    private final InstanceInfoMapper instanceInfoMapper;

    public InstanceInfoServiceImpl(final InstanceInfoMapper instanceInfoMapper) {
        this.instanceInfoMapper = instanceInfoMapper;
    }

    @Override
    @Transactional(propagation = Propagation.NOT_SUPPORTED)
    public void createOrUpdate(final InstanceInfoVO instanceInfoVO) {
        InstanceQuery instanceQuery = new InstanceQuery();
        instanceQuery.setInstanceIp(instanceInfoVO.getInstanceIp());
        instanceQuery.setInstancePort(instanceInfoVO.getInstancePort());
        instanceQuery.setInstanceType(instanceInfoVO.getInstanceType());
        instanceQuery.setNamespaceId(instanceInfoVO.getNamespaceId());
        InstanceInfoDO infoDO = instanceInfoMapper.selectOneByQuery(instanceQuery);
        if (Objects.isNull(infoDO)) {
            LOG.debug("Register new instance info: {}", GsonUtils.getInstance().toJson(instanceQuery));
            InstanceInfoDO instanceInfoDO = InstanceInfoDO.buildInstanceInfoDO(instanceInfoVO);
            try {
                instanceInfoMapper.insert(instanceInfoDO);
                return;
            } catch (DuplicateKeyException e) {
                // Another Admin may have registered this identity after our lookup.
                // Each mapper call runs outside a transaction so PostgreSQL can recover
                // from the failed insert without leaving an aborted transaction.
                infoDO = instanceInfoMapper.selectOneByQuery(instanceQuery);
                if (Objects.isNull(infoDO)) {
                    throw e;
                }
            } catch (Exception e) {
                LOG.error("Failed to register instance info", e);
                return;
            }
        }
        LOG.debug("Update instance info: {}", GsonUtils.getInstance().toJson(instanceInfoVO));
        infoDO.setInstanceIp(instanceInfoVO.getInstanceIp());
        infoDO.setInstanceType(instanceInfoVO.getInstanceType());
        infoDO.setInstanceInfo(instanceInfoVO.getInstanceInfo());
        infoDO.setNamespaceId(instanceInfoVO.getNamespaceId());
        infoDO.setDateUpdated(Timestamp.from(Instant.now()));
        infoDO.setInstanceState(instanceInfoVO.getInstanceState());
        infoDO.setLastHeartBeatTime(instanceInfoVO.getLastHeartBeatTime());
        instanceInfoMapper.updateById(infoDO);
    }

    @Pageable
    @Override
    public CommonPager<InstanceInfoVO> listByPage(final InstanceQuery instanceQuery) {
        List<InstanceInfoDO> instanceInfoDOList = instanceInfoMapper.selectByQuery(instanceQuery);
        return PageResultUtils.result(instanceQuery.getPageParameter(), () -> this.buildInstanceInfoVO(instanceInfoDOList));
    }

    @Override
    public List<InstanceInfoVO> list() {
        return this.buildInstanceInfoVO(instanceInfoMapper.selectAll());
    }

    @Override
    public InstanceInfoVO findById(final String id) {
        InstanceInfoDO instanceInfoDO = instanceInfoMapper.selectById(id);
        return Objects.isNull(instanceInfoDO) ? null : this.buildInstanceInfoVO(instanceInfoDO);
    }

    private List<InstanceInfoVO> buildInstanceInfoVO(final List<InstanceInfoDO> instanceInfoDOList) {
        if (instanceInfoDOList.isEmpty()) {
            return List.of();
        }
        return instanceInfoDOList.stream()
                .map(this::buildInstanceInfoVO)
                .toList();
    }

    private InstanceInfoVO buildInstanceInfoVO(final InstanceInfoDO instanceInfoDO) {
        InstanceInfoVO instanceInfoVO = new InstanceInfoVO();
        instanceInfoVO.setId(instanceInfoDO.getId());
        instanceInfoVO.setInstanceIp(instanceInfoDO.getInstanceIp());
        instanceInfoVO.setInstancePort(instanceInfoDO.getInstancePort());
        instanceInfoVO.setInstanceType(instanceInfoDO.getInstanceType());
        // Normalize legacy rows for display without rewriting the stored value.
        instanceInfoVO.setInstanceInfo(InstanceInfoUtils.normalize(instanceInfoDO.getInstanceInfo()));
        instanceInfoVO.setInstanceState(instanceInfoDO.getInstanceState());
        instanceInfoVO.setNamespaceId(instanceInfoDO.getNamespaceId());
        instanceInfoVO.setDateCreated(instanceInfoDO.getDateCreated());
        instanceInfoVO.setDateUpdated(instanceInfoDO.getDateUpdated());
        instanceInfoVO.setLastHeartBeatTime(instanceInfoDO.getLastHeartBeatTime());
        return instanceInfoVO;
    }
}
