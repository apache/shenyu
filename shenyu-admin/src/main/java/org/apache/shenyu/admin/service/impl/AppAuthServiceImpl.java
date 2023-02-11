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
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthParamDTO;
import org.apache.shenyu.admin.model.dto.AuthPathDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.AuthParamVO;
import org.apache.shenyu.admin.model.vo.AuthPathVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.transfer.AppAuthTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.SignUtils;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.AppAuthService}.
 */
@Service
public class AppAuthServiceImpl implements AppAuthService {

    private final AppAuthMapper appAuthMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final AuthParamMapper authParamMapper;

    private final AuthPathMapper authPathMapper;

    public AppAuthServiceImpl(final AppAuthMapper appAuthMapper,
                              final ApplicationEventPublisher eventPublisher,
                              final AuthParamMapper authParamMapper,
                              final AuthPathMapper authPathMapper) {
        this.appAuthMapper = appAuthMapper;
        this.eventPublisher = eventPublisher;
        this.authParamMapper = authParamMapper;
        this.authPathMapper = authPathMapper;
    }

    @Override
    public List<AppAuthVO> searchByCondition(final AppAuthQuery condition) {
        return appAuthMapper.selectByCondition(condition);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult applyCreate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isBlank(authApplyDTO.getAppName())
                || (authApplyDTO.getOpen() && CollectionUtils.isEmpty(authApplyDTO.getPathList()))) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        AppAuthDO appAuthDO = AppAuthDO.create(authApplyDTO);
        appAuthMapper.insert(appAuthDO);
        // save authParam
        AuthParamDO authParamDO = AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam());
        authParamMapper.save(authParamDO);

        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .paramDataList(Lists.newArrayList(new AuthParamData(authParamDO.getAppName(), authParamDO.getAppParam())))
                .build();

        // save authPath
        if (Boolean.TRUE.equals(appAuthDO.getOpen())) {
            List<AuthPathDO> collect = authApplyDTO.getPathList()
                    .stream()
                    .map(path -> AuthPathDO.create(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathMapper.batchSave(collect);
            data.setPathDataList(collect.stream().map(authPathDO ->
                    AuthPathData.builder().appName(authPathDO.getAppName()).path(authPathDO.getPath()).enabled(authPathDO.getEnabled()).build())
                    .collect(Collectors.toList()));
        }

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(data)));

        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS);
    }

    @Override
    public ShenyuAdminResult applyUpdate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isAnyBlank(authApplyDTO.getAppKey(), authApplyDTO.getAppName())
                || (authApplyDTO.getOpen() && CollectionUtils.isEmpty(authApplyDTO.getPathList()))) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        AppAuthDO appAuthDO = appAuthMapper.findByAppKey(authApplyDTO.getAppKey());
        if (Objects.isNull(appAuthDO)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.APPKEY_NOT_EXIST_ERROR);
        }

        AuthParamDO authParamDO = authParamMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
        if (Objects.isNull(authParamDO)) {
            // save authParam
            authParamMapper.save(AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam()));
        }

        if (Boolean.TRUE.equals(appAuthDO.getOpen())) {
            List<AuthPathDO> existList = authPathMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
            if (CollectionUtils.isNotEmpty(existList)) {
                authPathMapper.deleteByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
            }
            List<AuthPathDO> collect = authApplyDTO.getPathList()
                    .stream()
                    .map(path -> AuthPathDO.create(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathMapper.batchSave(collect);
        }

        // publish create Event of APP_AUTH
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(buildByEntity(appAuthDO))));

        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult updateDetail(final AppAuthDTO appAuthDTO) {
        AppAuthDO appAuthDO = AppAuthTransfer.INSTANCE.mapToEntity(appAuthDTO);
        appAuthMapper.update(appAuthDO);
        List<AuthParamDTO> authParamDTOList = appAuthDTO.getAuthParamDTOList();
        if (CollectionUtils.isNotEmpty(authParamDTOList)) {
            authParamMapper.deleteByAuthId(appAuthDTO.getId());

            List<AuthParamDO> authParamDOList = authParamDTOList.stream()
                    .map(dto -> AuthParamDO.create(appAuthDTO.getId(), dto.getAppName(), dto.getAppParam()))
                    .collect(Collectors.toList());
            authParamMapper.batchSave(authParamDOList);
        }
        List<AuthPathDTO> authPathDTOList = appAuthDTO.getAuthPathDTOList();
        if (CollectionUtils.isNotEmpty(authPathDTOList)) {
            List<AuthPathDO> oldAuthPathDOList = authPathMapper.findByAuthId(appAuthDTO.getId());
            String appName = oldAuthPathDOList.stream().findFirst()
                    .map(AuthPathDO::getAppName).orElse(StringUtils.EMPTY);

            authPathMapper.deleteByAuthId(appAuthDTO.getId());

            List<AuthPathDO> authPathDOList = authPathDTOList.stream()
                    .filter(Objects::nonNull)
                    .map(dto -> AuthPathDO.create(dto.getPath(), appAuthDTO.getId(), appName))
                    .collect(Collectors.toList());
            authPathMapper.batchSave(authPathDOList);
        }

        AppAuthData appAuthData = buildByEntity(appAuthDO);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(appAuthData)));
        return ShenyuAdminResult.success();
    }

    @Override
    public ShenyuAdminResult updateDetailPath(final AuthPathWarpDTO authPathWarpDTO) {
        AppAuthDO appAuthDO = appAuthMapper.selectById(authPathWarpDTO.getId());
        if (Objects.isNull(appAuthDO)) {
            return ShenyuAdminResult.error(AdminConstants.ID_NOT_EXIST);
        }
        List<AuthPathDTO> authPathDTOList = authPathWarpDTO.getAuthPathDTOList();
        if (CollectionUtils.isNotEmpty(authPathDTOList)) {
            authPathMapper.deleteByAuthId(authPathWarpDTO.getId());

            List<AuthPathDO> collect = authPathDTOList.stream()
                    .filter(Objects::nonNull)
                    .map(authPathDTO -> AuthPathDO.create(authPathDTO.getPath(), appAuthDO.getId(), authPathDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathMapper.batchSave(collect);
        }
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(buildByEntity(appAuthDO))));
        return ShenyuAdminResult.success();
    }

    @Override
    public ShenyuAdminResult syncData() {
        List<AppAuthDO> appAuthDOList = appAuthMapper.selectAll();
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return ShenyuAdminResult.success();
        }

        List<String> idList = appAuthDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        Map<String, List<AuthParamData>> paramMap = this.prepareAuthParamData(idList);
        Map<String, List<AuthPathData>> pathMap = this.prepareAuthPathData(idList);

        List<AppAuthData> dataList = appAuthDOList.stream()
                .filter(Objects::nonNull)
                .map(appAuthDO -> {
                    String id = appAuthDO.getId();
                    return buildByEntityWithParamAndPath(appAuthDO, paramMap.get(id), pathMap.get(id));
                })
                .collect(Collectors.toList());
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.REFRESH,
                dataList));

        return ShenyuAdminResult.success();
    }


    /**
     * create or update application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows
     */
    @Override
    public int createOrUpdate(final AppAuthDTO appAuthDTO) {
        int appAuthCount;
        AppAuthDO appAuthDO = AppAuthDO.create(appAuthDTO);
        DataEventTypeEnum eventType;
        if (StringUtils.isBlank(appAuthDTO.getId())) {
            appAuthDO.setAppSecret(SignUtils.generateKey());
            appAuthCount = appAuthMapper.insertSelective(appAuthDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            appAuthCount = appAuthMapper.updateSelective(appAuthDO);
            eventType = DataEventTypeEnum.UPDATE;
        }
        // publish AppAuthData's event
        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .paramDataList(null)
                .pathDataList(null)
                .build();
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, eventType, Collections.singletonList(data)));

        return appAuthCount;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return 0;
        }
        List<AppAuthDO> appAuthList = appAuthMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(appAuthList)) {
            return 0;
        }
        int affectCount = appAuthMapper.deleteByIds(ids);
        if (affectCount <= 0) {
            return affectCount;
        }
        authParamMapper.deleteByAuthIds(ids);
        authPathMapper.deleteByAuthIds(ids);

        List<AppAuthData> appAuthData = appAuthList.stream().map(appAuthDO -> AppAuthData.builder()
            .appKey(appAuthDO.getAppKey())
            .appSecret(appAuthDO.getAppSecret())
            .open(appAuthDO.getOpen())
            .enabled(appAuthDO.getEnabled())
            .paramDataList(null)
            .pathDataList(null)
            .build()).collect(Collectors.toCollection(() -> new ArrayList<>(appAuthList.size())));
        // publish delete event of AppAuthData
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.DELETE, appAuthData));

        return affectCount;
    }

    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {
        List<String> distinctIds = ids.stream().distinct().collect(Collectors.toList());
        List<AppAuthDO> appAuthDOList = appAuthMapper.selectByIds(distinctIds);
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return AdminConstants.ID_NOT_EXIST;
        }

        Map<String, List<AuthParamData>> paramMap = this.prepareAuthParamData(distinctIds);
        Map<String, List<AuthPathData>> pathMap = this.prepareAuthPathData(distinctIds);

        List<AppAuthData> authDataList = appAuthDOList.stream().map(appAuthDO -> {
            String id = appAuthDO.getId();
            appAuthDO.setEnabled(enabled);
            return this.buildByEntityWithParamAndPath(appAuthDO, paramMap.get(id), pathMap.get(id));
        }).collect(Collectors.toList());

        appAuthMapper.updateEnableBatch(distinctIds, enabled);

        // publish change event.
        if (CollectionUtils.isNotEmpty(authDataList)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.UPDATE,
                    authDataList));
        }
        return StringUtils.EMPTY;
    }

    /**
     * find application authority by id.
     *
     * @param id primary key.
     * @return {@linkplain AppAuthVO}
     */
    @Override
    public AppAuthVO findById(final String id) {
        AppAuthVO appAuthVO = AppAuthTransfer.INSTANCE.mapToVO(appAuthMapper.selectById(id));
        List<AuthParamDO> authParamDOList = authParamMapper.findByAuthId(id);
        if (CollectionUtils.isNotEmpty(authParamDOList)) {
            appAuthVO.setAuthParamVOList(authParamDOList.stream().map(authParamDO -> {
                AuthParamVO vo = new AuthParamVO();
                vo.setAppName(authParamDO.getAppName());
                vo.setAppParam(authParamDO.getAppParam());
                return vo;
            }).collect(Collectors.toList()));
        }
        appAuthVO.setAuthPathVOList(detailPath(id));
        return appAuthVO;
    }

    @Override
    public List<AuthPathVO> detailPath(final String authId) {
        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthId(authId);
        if (CollectionUtils.isEmpty(authPathDOList)) {
            return new ArrayList<>();
        }

        return authPathDOList.stream().map(authPathDO -> {
            AuthPathVO vo = new AuthPathVO();
            vo.setId(authPathDO.getId());
            vo.setAppName(authPathDO.getAppName());
            vo.setPath(authPathDO.getPath());
            vo.setEnabled(authPathDO.getEnabled());
            return vo;
        }).collect(Collectors.toList());

    }

    /**
     * find page of application authority by query.
     *
     * @param appAuthQuery {@linkplain AppAuthQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<AppAuthVO> listByPage(final AppAuthQuery appAuthQuery) {
        return PageResultUtils.result(appAuthQuery.getPageParameter(),
            () -> appAuthMapper.countByQuery(appAuthQuery),
            () -> appAuthMapper.selectByQuery(appAuthQuery)
                        .stream()
                        .map(AppAuthTransfer.INSTANCE::mapToVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public List<AppAuthData> listAll() {
        List<AppAuthDO> appAuthDOList = appAuthMapper.selectAll();
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return new ArrayList<>();
        }

        List<String> idList = appAuthDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        Map<String, List<AuthParamData>> paramMap = this.prepareAuthParamData(idList);
        Map<String, List<AuthPathData>> pathMap = this.prepareAuthPathData(idList);

        return appAuthDOList.stream().map(appAuthDO -> {
            String id = appAuthDO.getId();
            return buildByEntityWithParamAndPath(appAuthDO, paramMap.get(id), pathMap.get(id));
        }).collect(Collectors.toList());
    }

    @Override
    public ShenyuAdminResult updateAppSecretByAppKey(final String appKey, final String appSecret) {
        return ShenyuAdminResult.success(appAuthMapper.updateAppSecretByAppKey(appKey, appSecret));
    }

    @Override
    public AppAuthDO findByAppKey(final String appKey) {
        return appAuthMapper.findByAppKey(appKey);
    }

    private AppAuthData buildByEntity(final AppAuthDO appAuthDO) {
        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .build();
        List<AuthParamDO> authParamDOList = authParamMapper.findByAuthId(appAuthDO.getId());
        if (CollectionUtils.isNotEmpty(authParamDOList)) {
            data.setParamDataList(
                    authParamDOList.stream()
                            .map(paramDO -> new AuthParamData(paramDO.getAppName(), paramDO.getAppParam()))
                            .collect(Collectors.toList())
            );
        }
        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthId(appAuthDO.getId());
        if (CollectionUtils.isNotEmpty(authPathDOList)) {
            data.setPathDataList(
                    authPathDOList.stream()
                            .map(pathDO -> new AuthPathData(pathDO.getAppName(), pathDO.getPath(), pathDO.getEnabled()))
                            .collect(Collectors.toList()));
        }
        return data;
    }

    private AppAuthData buildByEntityWithParamAndPath(final AppAuthDO appAuthDO, final List<AuthParamData> authParamDataList, final List<AuthPathData> authPathDataList) {
        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .build();
        if (CollectionUtils.isNotEmpty(authParamDataList)) {
            data.setParamDataList(authParamDataList);
        }
        if (CollectionUtils.isNotEmpty(authPathDataList)) {
            data.setPathDataList(authPathDataList);
        }
        return data;
    }

    /**
     * prepare the Map with authIds.
     * @param authIds auth id
     * @return a map consist of param info
     */
    private Map<String, List<AuthParamData>> prepareAuthParamData(final List<String> authIds) {

        List<AuthParamDO> authPathDOList = authParamMapper.findByAuthIdList(authIds);

        return Optional.ofNullable(authPathDOList).orElseGet(ArrayList::new)
                .stream().collect(Collectors.toMap(AuthParamDO::getAuthId,
                    data -> {
                        List<AuthParamData> dataList = new ArrayList<>();
                        dataList.add(new AuthParamData(data.getAppName(), data.getAppParam()));
                        return dataList;
                    }, (List<AuthParamData> dataList1, List<AuthParamData> dataList2) -> {
                        dataList1.addAll(dataList2);
                        return dataList1;
                    }));
    }

    /**
     * prepare the Map with authIds.
     * @param authIds auth id
     * @return a map consist of path info
     */
    private Map<String, List<AuthPathData>> prepareAuthPathData(final List<String> authIds) {

        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthIdList(authIds);
        return Optional.ofNullable(authPathDOList).orElseGet(ArrayList::new)
                .stream().collect(Collectors.toMap(AuthPathDO::getAuthId,
                    data -> {
                        List<AuthPathData> dataList = new ArrayList<>();
                        dataList.add(new AuthPathData(data.getAppName(), data.getPath(), data.getEnabled()));
                        return dataList;
                    }, (List<AuthPathData> dataList1, List<AuthPathData> dataList2) -> {
                        dataList1.addAll(dataList2);
                        return dataList1;
                    }));
    }

}
