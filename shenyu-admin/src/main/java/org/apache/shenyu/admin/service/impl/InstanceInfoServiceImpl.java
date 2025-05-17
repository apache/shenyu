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

import org.apache.shenyu.admin.mapper.InstanceInfoMapper;
import org.apache.shenyu.admin.model.dto.InstanceInfoDTO;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.InstanceInfoService}.
 */
@Service
public class InstanceInfoServiceImpl implements InstanceInfoService {

    private static final Logger LOG = LoggerFactory.getLogger(InstanceInfoServiceImpl.class);

    private static final long HEARTBEAT_TIMEOUT = 10_000;

    private final InstanceInfoMapper instanceInfoMapper;

    private final ConcurrentHashMap<String, Long> bootStrapStatus = new ConcurrentHashMap<>();

    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);

    public InstanceInfoServiceImpl(final InstanceInfoMapper instanceInfoMapper) {
        this.instanceInfoMapper = instanceInfoMapper;
        scheduler.scheduleAtFixedRate(this::checkTimeouts, 0, 60, TimeUnit.SECONDS);
        List<InstanceInfoDO> instanceInfoDOS = this.instanceInfoMapper.selectAll();
        instanceInfoDOS.forEach(instanceInfoDO -> {
            bootStrapStatus.put(instanceInfoDO.getId(), instanceInfoDO.getDateUpdated().getTime());
        });
    }

    @Override
    public void registerInstanceInfo(final InstanceInfoReportEvent instanceInfoRegisterDTO) {
        LOG.info("Instance info registered: {}", GsonUtils.getInstance().toJson(instanceInfoRegisterDTO));
        InstanceInfoDTO instanceInfoDTO = buildInstanceInfoDTO(instanceInfoRegisterDTO);
        createOrUpdate(instanceInfoDTO);
    }

    @Override
    public void createOrUpdate(final InstanceInfoDTO instanceInfoDTO) {
        InstanceQuery instanceQuery = new InstanceQuery();
        instanceQuery.setInstanceIp(instanceInfoDTO.getInstanceIp());
        instanceQuery.setInstancePort(instanceInfoDTO.getInstancePort());
        instanceQuery.setInstanceType(instanceInfoDTO.getInstanceType());
        instanceQuery.setNamespaceId(instanceInfoDTO.getNamespaceId());
        InstanceInfoDO infoDO = instanceInfoMapper.selectOneByQuery(instanceQuery);
        if (Objects.isNull(infoDO)) {
            LOG.info("Register new instance info: {}", GsonUtils.getInstance().toJson(instanceInfoDTO));
            InstanceInfoDO instanceInfoDO = InstanceInfoDO.buildInstanceInfoDO(instanceInfoDTO);
            try {
                instanceInfoMapper.insert(instanceInfoDO);
                bootStrapStatus.put(instanceInfoDO.getId(), System.currentTimeMillis());
            } catch (Exception e) {
                LOG.error("Failed to register instance info", e);
            }
            return;
        }
        LOG.info("Update instance info: {}", GsonUtils.getInstance().toJson(instanceInfoDTO));
        infoDO.setInstanceIp(instanceInfoDTO.getInstanceIp());
        infoDO.setInstanceType(instanceInfoDTO.getInstanceType());
        infoDO.setInstanceInfo(instanceInfoDTO.getInstanceInfo());
        infoDO.setNamespaceId(instanceInfoDTO.getNamespaceId());
        infoDO.setDateUpdated(Timestamp.from(Instant.now()));
        infoDO.setInstanceState(instanceInfoDTO.getInstanceState());
        instanceInfoMapper.updateById(infoDO);
        bootStrapStatus.put(infoDO.getId(), System.currentTimeMillis());
    }

    @Override
    public CommonPager<InstanceInfoVO> listByPage(final InstanceQuery instanceQuery) {
        List<InstanceInfoDO> instanceInfoDOList = instanceInfoMapper.selectByQuery(instanceQuery);
        return PageResultUtils.result(instanceQuery.getPageParameter(), () -> this.buildInstanceInfoVO(instanceInfoDOList));
    }

    @Override
    public InstanceInfoVO findById(final String id) {
        return null;
    }

    @Override
    public void offline(final InstanceInfoDTO instanceInfoDTO) {
        InstanceQuery instanceQuery = new InstanceQuery();
        instanceQuery.setInstanceIp(instanceInfoDTO.getInstanceIp());
        instanceQuery.setInstancePort(instanceInfoDTO.getInstancePort());
        instanceQuery.setInstanceType(instanceInfoDTO.getInstanceType());
        instanceQuery.setInstanceId(instanceInfoDTO.getInstanceId());
        instanceQuery.setNamespaceId(instanceInfoDTO.getNamespaceId());
        InstanceInfoDO infoDO = instanceInfoMapper.selectOneByQuery(instanceQuery);
        if (!Objects.isNull(infoDO)) {
            infoDO.setInstanceState(2);
            instanceInfoMapper.updateById(infoDO);
            bootStrapStatus.remove(infoDO.getId());
            LOG.info("Offline instance info: {}", GsonUtils.getInstance().toJson(infoDO.getInstanceInfo()));
        }
    }

    private InstanceInfoDTO buildInstanceInfoDTO(final InstanceInfoReportEvent instanceInfoRegisterDTO) {
        InstanceInfoDTO instanceInfoDTO = new InstanceInfoDTO();
        instanceInfoDTO.setInstanceIp(instanceInfoRegisterDTO.getInstanceIp());
        instanceInfoDTO.setInstancePort(instanceInfoRegisterDTO.getInstancePort());
        instanceInfoDTO.setInstanceType(instanceInfoRegisterDTO.getInstanceType());
        instanceInfoDTO.setInstanceInfo(instanceInfoRegisterDTO.getInstanceInfo());
        instanceInfoDTO.setInstanceState(instanceInfoRegisterDTO.getInstanceState());
        instanceInfoDTO.setNamespaceId(instanceInfoRegisterDTO.getNamespaceId());
        return instanceInfoDTO;
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
        instanceInfoVO.setInstanceIp(instanceInfoDO.getInstanceIp());
        instanceInfoVO.setInstancePort(instanceInfoDO.getInstancePort());
        instanceInfoVO.setInstanceType(instanceInfoDO.getInstanceType());
        instanceInfoVO.setInstanceInfo(instanceInfoDO.getInstanceInfo());
        instanceInfoVO.setNamespaceId(instanceInfoDO.getNamespaceId());
        instanceInfoVO.setDateCreated(instanceInfoDO.getDateCreated());
        instanceInfoVO.setDateUpdated(instanceInfoDO.getDateUpdated());
        return instanceInfoVO;
    }

    /**
     * listen {@link InstanceInfoReportEvent} instance info report event.
     *
     * @param event event
     */
    @EventListener(InstanceInfoReportEvent.class)
    public void onInstanceInfoReport(final InstanceInfoReportEvent event) {
        InstanceInfoDTO instanceInfoDTO = buildInstanceInfoDTO(event);
        createOrUpdate(instanceInfoDTO);
    }

    private void checkTimeouts() {
        long currentTimeMillis = System.currentTimeMillis();
        bootStrapStatus.forEach((bootstrapId, lastHeartbeats) -> {
            if (currentTimeMillis - lastHeartbeats > HEARTBEAT_TIMEOUT) {
                InstanceInfoDTO instanceInfoDTO = new InstanceInfoDTO();
                instanceInfoDTO.setInstanceId(bootstrapId);
                this.offline(instanceInfoDTO);
            }
        });
    }
}
