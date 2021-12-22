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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.transfer.MetaDataTransfer;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.MetaDataService}.
 */
@Service
public class MetaDataServiceImpl implements MetaDataService {

    private static final Logger LOG = LoggerFactory.getLogger(MetaDataServiceImpl.class);

    private final MetaDataMapper metaDataMapper;

    private final ApplicationEventPublisher eventPublisher;

    public MetaDataServiceImpl(final MetaDataMapper metaDataMapper, final ApplicationEventPublisher eventPublisher) {
        this.metaDataMapper = metaDataMapper;
        this.eventPublisher = eventPublisher;
    }

    @Override
    public void saveOrUpdateMetaData(final MetaDataDO exist, final MetaDataRegisterDTO metaDataDTO) {
        DataEventTypeEnum eventType;
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapRegisterDTOToEntity(metaDataDTO);
        if (Objects.isNull(exist)) {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            metaDataDO.setId(UUIDUtils.getInstance().generateShortUuid());
            metaDataDO.setDateCreated(currentTime);
            metaDataDO.setDateUpdated(currentTime);
            metaDataMapper.insert(metaDataDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            metaDataDO.setId(exist.getId());
            metaDataMapper.update(metaDataDO);
            eventType = DataEventTypeEnum.UPDATE;
        }
        // publish MetaData's event
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, eventType,
                Collections.singletonList(MetaDataTransfer.INSTANCE.mapToData(metaDataDO))));
    }

    @Override
    public String createOrUpdate(final MetaDataDTO metaDataDTO) {
        String msg = checkData(metaDataDTO);
        if (StringUtils.isNoneBlank(msg)) {
            return msg;
        }
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        DataEventTypeEnum eventType;
        String pathDesc = Objects.isNull(metaDataDO.getPathDesc()) ? "" : metaDataDO.getPathDesc();
        if (StringUtils.isEmpty(metaDataDTO.getId())) {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            metaDataDO.setId(UUIDUtils.getInstance().generateShortUuid());
            metaDataDO.setPathDesc(pathDesc);
            metaDataDO.setDateCreated(currentTime);
            metaDataDO.setDateUpdated(currentTime);
            metaDataMapper.insert(metaDataDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            MetaDataDO m = metaDataMapper.selectById(metaDataDTO.getId());
            Optional.ofNullable(m).ifPresent(e -> metaDataDTO.setEnabled(e.getEnabled()));
            metaDataDO.setPathDesc(pathDesc);
            metaDataMapper.update(metaDataDO);
            eventType = DataEventTypeEnum.UPDATE;
        }
        // publish AppAuthData's event
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, eventType,
                Collections.singletonList(MetaDataTransfer.INSTANCE.mapToData(metaDataDTO))));
        return StringUtils.EMPTY;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {

        int count = 0;
        Set<String> idSet = Optional.ofNullable(ids).orElseGet(() -> new ArrayList<>())
                .stream().filter(id -> StringUtils.isNotEmpty(id)).collect(Collectors.toSet());
        if (CollectionUtils.isNotEmpty(idSet)) {
            List<MetaDataDO> metaDataDoList = metaDataMapper.selectByIdSet(idSet);
            List<MetaData> metaDataList = Optional.ofNullable(metaDataDoList).orElseGet(() -> new ArrayList<>())
                    .stream().map(metaDataDO -> MetaDataTransfer.INSTANCE.mapToData(metaDataDO)).collect(Collectors.toList());

            count = metaDataMapper.deleteByIdSet(idSet);
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.DELETE, metaDataList));
        }

        return count;
    }

    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {

        Set<String> idSet = Optional.ofNullable(ids).orElseGet(() -> new ArrayList<>())
                .stream().filter(id -> StringUtils.isNotEmpty(id)).collect(Collectors.toSet());
        if (CollectionUtils.isEmpty(idSet)) {
            return AdminConstants.ID_NOT_EXIST;
        }
        List<MetaDataDO> metaDataDoList = Optional.ofNullable(metaDataMapper.selectByIdSet(idSet)).orElseGet(() -> new ArrayList<>());
        if (idSet.size() != metaDataDoList.size()) {
            return AdminConstants.ID_NOT_EXIST;
        }
        List<MetaData> metaDataList = metaDataDoList.stream().map(metaDataDO -> MetaDataTransfer.INSTANCE.mapToData(metaDataDO))
                .collect(Collectors.toList());
        metaDataMapper.updateEnableBatch(idSet, enabled);

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.UPDATE,
                metaDataList));

        return StringUtils.EMPTY;
    }

    @Override
    public void syncData() {
        List<MetaDataDO> all = metaDataMapper.findAll();
        if (CollectionUtils.isNotEmpty(all)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.META_DATA, DataEventTypeEnum.REFRESH, MetaDataTransfer.INSTANCE.mapToDataAll(all)));
        }
    }

    @Override
    public MetaDataVO findById(final String id) {
        return Optional.ofNullable(MetaDataTransfer.INSTANCE.mapToVO(metaDataMapper.selectById(id))).orElseGet(MetaDataVO::new);
    }

    @Override
    @Pageable
    public CommonPager<MetaDataVO> listByPage(final MetaDataQuery metaDataQuery) {
        return PageResultUtils.result(metaDataQuery.getPageParameter(), () -> metaDataMapper.selectByQuery(metaDataQuery)
                .stream()
                .map(MetaDataTransfer.INSTANCE::mapToVO)
                .collect(Collectors.toList()));
    }

    @Override
    public List<MetaDataVO> findAll() {
        return MetaDataTransfer.INSTANCE.mapToVOList(metaDataMapper.selectAll());
    }

    @Override
    public Map<String, List<MetaDataVO>> findAllGroup() {
        List<MetaDataVO> metaDataVOS = MetaDataTransfer.INSTANCE.mapToVOList(metaDataMapper.selectAll());
        return metaDataVOS.stream().collect(Collectors.groupingBy(MetaDataVO::getAppName));
    }

    @Override
    public List<MetaData> listAll() {
        return metaDataMapper.selectAll()
                .stream()
                .filter(Objects::nonNull)
                .map(MetaDataTransfer.INSTANCE::mapToData)
                .collect(Collectors.toList());
    }

    @Override
    public MetaDataDO findByPath(final String path) {
        return metaDataMapper.findByPath(path);
    }

    @Override
    public MetaDataDO findByServiceNameAndMethodName(final String serviceName, final String methodName) {
        return metaDataMapper.findByServiceNameAndMethod(serviceName, methodName);
    }

    @Override
    public int insert(final MetaDataDO metaDataDO) {
        return metaDataMapper.insert(metaDataDO);
    }

    private String checkData(final MetaDataDTO metaDataDTO) {
        Boolean success = checkParam(metaDataDTO);
        if (!success) {
            LOG.error("metaData create param is error, {}", metaDataDTO);
            return AdminConstants.PARAMS_ERROR;
        }

        final MetaDataDO exist = metaDataMapper.findByPath(metaDataDTO.getPath());
        if (Objects.nonNull(exist) && !exist.getId().equals(metaDataDTO.getId())) {
            return AdminConstants.DATA_PATH_IS_EXIST;
        }

        return StringUtils.EMPTY;
    }

    private Boolean checkParam(final MetaDataDTO metaDataDTO) {
        return !StringUtils.isEmpty(metaDataDTO.getAppName())
                && !StringUtils.isEmpty(metaDataDTO.getPath())
                && !StringUtils.isEmpty(metaDataDTO.getRpcType())
                && !StringUtils.isEmpty(metaDataDTO.getServiceName())
                && !StringUtils.isEmpty(metaDataDTO.getMethodName());
    }
}
