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
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.utils.SoulResultMessage;
import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthParamDTO;
import org.apache.shenyu.admin.model.dto.AuthPathDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.SoulAdminResult;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.transfer.AppAuthTransfer;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.AuthParamVO;
import org.apache.shenyu.admin.model.vo.AuthPathVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.SignUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * AppAuthServiceImpl.
 *
 * @author xiaoyu(Myth)
 * @author nuo-promise
 */
@Service("appAuthService")
public class AppAuthServiceImpl implements AppAuthService {

    private final AppAuthMapper appAuthMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final AuthParamMapper authParamMapper;

    private final AuthPathMapper authPathMapper;

    @Autowired(required = false)
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
    @Transactional
    public SoulAdminResult applyCreate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isBlank(authApplyDTO.getAppName())
                || (authApplyDTO.getOpen() && CollectionUtils.isEmpty(authApplyDTO.getPathList()))) {
            return SoulAdminResult.error(SoulResultMessage.PARAMETER_ERROR);
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
        if (appAuthDO.getOpen()) {
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

        return SoulAdminResult.success(SoulResultMessage.CREATE_SUCCESS);
    }

    @Override
    public SoulAdminResult applyUpdate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isBlank(authApplyDTO.getAppKey())
                || StringUtils.isBlank(authApplyDTO.getAppName())
                || (authApplyDTO.getOpen() && CollectionUtils.isEmpty(authApplyDTO.getPathList()))) {
            return SoulAdminResult.error(SoulResultMessage.PARAMETER_ERROR);
        }
        AppAuthDO appAuthDO = appAuthMapper.findByAppKey(authApplyDTO.getAppKey());
        if (Objects.isNull(appAuthDO)) {
            return SoulAdminResult.error(SoulResultMessage.APPKEY_NOT_EXIST_ERROR);
        }

        AuthParamDO authParamDO = authParamMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
        if (Objects.isNull(authParamDO)) {
            // save authParam
            authParamMapper.save(AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam()));
        }

        if (appAuthDO.getOpen()) {
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

        return SoulAdminResult.success(SoulResultMessage.UPDATE_SUCCESS);
    }

    @Override
    public SoulAdminResult updateDetail(final AppAuthDTO appAuthDTO) {
        if (StringUtils.isBlank(appAuthDTO.getAppKey())
                || StringUtils.isBlank(appAuthDTO.getAppSecret())
                || StringUtils.isBlank(appAuthDTO.getId())) {
            return SoulAdminResult.error(SoulResultMessage.PARAMETER_ERROR);
        }
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
        AppAuthData appAuthData = buildByEntity(appAuthDO);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(appAuthData)));
        return SoulAdminResult.success();
    }

    @Override
    public SoulAdminResult updateDetailPath(final AuthPathWarpDTO authPathWarpDTO) {
        AppAuthDO appAuthDO = appAuthMapper.selectById(authPathWarpDTO.getId());
        if (Objects.isNull(appAuthDO)) {
            return SoulAdminResult.error(AdminConstants.ID_NOT_EXIST);
        }
        List<AuthPathDTO> authPathDTOList = authPathWarpDTO.getAuthPathDTOList();
        if (CollectionUtils.isNotEmpty(authPathDTOList)) {
            authPathMapper.deleteByAuthId(authPathWarpDTO.getId());
            List<AuthPathDO> collect = authPathDTOList.stream()
                    .map(authPathDTO -> AuthPathDO.create(authPathDTO.getPath(), appAuthDO.getId(), authPathDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathMapper.batchSave(collect);
        }
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(buildByEntity(appAuthDO))));
        return SoulAdminResult.success();
    }

    @Override
    public SoulAdminResult syncData() {
        List<AppAuthDO> appAuthDOList = appAuthMapper.selectAll();
        if (CollectionUtils.isNotEmpty(appAuthDOList)) {
            List<AppAuthData> dataList = appAuthDOList.stream().map(this::buildByEntity).collect(Collectors.toList());
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.REFRESH,
                    dataList));
        }
        return SoulAdminResult.success();
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
        if (StringUtils.isEmpty(appAuthDTO.getId())) {
            appAuthDO.setAppSecret(SignUtils.getInstance().generateKey());
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
    public int delete(final List<String> ids) {
        int appAuthCount = 0;
        for (String id : ids) {
            final AppAuthDO appAuthDO = appAuthMapper.selectById(id);
            appAuthCount += appAuthMapper.delete(id);
            authParamMapper.deleteByAuthId(id);
            authPathMapper.deleteByAuthId(id);

            // publish delete event of AppAuthData
            AppAuthData data = AppAuthData.builder()
                    .appKey(appAuthDO.getAppKey())
                    .appSecret(appAuthDO.getAppSecret())
                    .open(appAuthDO.getOpen())
                    .enabled(appAuthDO.getEnabled())
                    .paramDataList(null)
                    .pathDataList(null)
                    .build();
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.DELETE, Collections.singletonList(data)));
        }
        return appAuthCount;
    }

    @Override
    public String enabled(final List<String> ids, final Boolean enabled) {
        List<AppAuthData> authDataList = Lists.newArrayList();
        for (String id : ids) {
            AppAuthDO appAuthDO = appAuthMapper.selectById(id);
            if (Objects.isNull(appAuthDO)) {
                return AdminConstants.ID_NOT_EXIST;
            }
            appAuthDO.setEnabled(enabled);
            appAuthMapper.updateEnable(appAuthDO);
            authDataList.add(buildByEntity(appAuthDO));
        }
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.UPDATE,
                authDataList));
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
        return appAuthVO;
    }

    @Override
    public List<AuthPathVO> detailPath(final String id) {
        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthId(id);
        if (CollectionUtils.isNotEmpty(authPathDOList)) {
            return authPathDOList.stream().map(authPathDO -> {
                AuthPathVO vo = new AuthPathVO();
                vo.setId(authPathDO.getId());
                vo.setAppName(authPathDO.getAppName());
                vo.setPath(authPathDO.getPath());
                vo.setEnabled(authPathDO.getEnabled());
                return vo;
            }).collect(Collectors.toList());
        }
        return new ArrayList<>();
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
        return appAuthMapper.selectAll()
                .stream()
                .map(appAuthDO -> buildByEntity(appAuthDO))
                .collect(Collectors.toList());
    }

    @Override
    public SoulAdminResult updateAppSecretByAppKey(final String appKey, final String appSecret) {
        return SoulAdminResult.success(appAuthMapper.updateAppSecretByAppKey(appKey, appSecret));
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
                            .collect(Collectors.toList())
            );
        }
        return data;
    }
}
