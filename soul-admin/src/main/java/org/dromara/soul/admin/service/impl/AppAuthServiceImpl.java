/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.admin.service.impl;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.dto.AuthApplyDTO;
import org.dromara.soul.admin.dto.AuthParamDTO;
import org.dromara.soul.admin.dto.AuthPathDTO;
import org.dromara.soul.admin.dto.AuthPathWarpDTO;
import org.dromara.soul.admin.entity.AppAuthDO;
import org.dromara.soul.admin.entity.AuthParamDO;
import org.dromara.soul.admin.entity.AuthPathDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.AppAuthMapper;
import org.dromara.soul.admin.mapper.AuthParamMapper;
import org.dromara.soul.admin.mapper.AuthPathMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.transfer.AppAuthTransfer;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.dromara.soul.admin.vo.AuthParamVO;
import org.dromara.soul.admin.vo.AuthPathVO;
import org.dromara.soul.common.constant.AdminConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.AuthParamData;
import org.dromara.soul.common.dto.AuthPathData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.common.utils.UUIDUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * AppAuthServiceImpl.
 *
 * @author xiaoyu(Myth)
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
                || CollectionUtils.isEmpty(authApplyDTO.getPathList())) {
            return SoulAdminResult.error("参数错误");
        }
        AppAuthDO appAuthDO = new AppAuthDO();
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        appAuthDO.setId(UUIDUtils.getInstance().generateShortUuid());
        appAuthDO.setUserId(authApplyDTO.getUserId());
        appAuthDO.setPhone(authApplyDTO.getPhone());
        appAuthDO.setExtInfo(authApplyDTO.getExtInfo());
        appAuthDO.setAppKey(SignUtils.getInstance().generateKey());
        appAuthDO.setAppSecret(SignUtils.getInstance().generateKey());
        appAuthDO.setEnabled(true);
        appAuthDO.setDateUpdated(currentTime);
        appAuthDO.setDateCreated(currentTime);
        appAuthMapper.insert(appAuthDO);
        //保存业务参数
        AuthParamDO authParamDO = buildAuthParamDO(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam());
        authParamMapper.save(authParamDO);
        //保存申请的path
        List<AuthPathDO> collect = authApplyDTO.getPathList()
                .stream()
                .map(path -> buildAuthPathDO(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                .collect(Collectors.toList());
        authPathMapper.batchSave(collect);

        AppAuthData data = new AppAuthData();
        data.setAppKey(appAuthDO.getAppKey());
        data.setAppSecret(appAuthDO.getAppSecret());
        data.setEnabled(appAuthDO.getEnabled());

        data.setParamDataList(Lists.newArrayList(new AuthParamData(authParamDO.getAppName(), authParamDO.getAppParam())));

        data.setPathDataList(collect.stream().map(authPathDO ->
                new AuthPathData(authPathDO.getAppName(), authPathDO.getPath(), authPathDO.getEnabled()))
                .collect(Collectors.toList()));

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(data)));

        return SoulAdminResult.success("申请成功！");
    }

    @Override
    public SoulAdminResult applyUpdate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isBlank(authApplyDTO.getAppKey())
                || StringUtils.isBlank(authApplyDTO.getAppName())
                || CollectionUtils.isEmpty(authApplyDTO.getPathList())) {
            return SoulAdminResult.error("参数错误");
        }
        AppAuthDO appAuthDO = appAuthMapper.findByAppKey(authApplyDTO.getAppKey());
        if (Objects.isNull(appAuthDO)) {
            return SoulAdminResult.error("传入的AppKey不存在!");
        }

        AuthParamDO authParamDO = authParamMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
        if (Objects.isNull(authParamDO)) {
            //保存业务参数
            authParamMapper.save(buildAuthParamDO(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam()));
        }
        List<AuthPathDO> existList = authPathMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
        if (CollectionUtils.isNotEmpty(existList)) {
            //删除后
            authPathMapper.deleteByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
        }
        //新增
        List<AuthPathDO> collect = authApplyDTO.getPathList()
                .stream()
                .map(path -> buildAuthPathDO(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                .collect(Collectors.toList());
        authPathMapper.batchSave(collect);

        //发送响应事件
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(buildByEntity(appAuthDO))));

        return SoulAdminResult.success("修改成功！");
    }

    @Override
    public SoulAdminResult updateDetail(final AppAuthDTO appAuthDTO) {
        if (StringUtils.isBlank(appAuthDTO.getAppKey())
                || StringUtils.isBlank(appAuthDTO.getAppSecret())
                || StringUtils.isBlank(appAuthDTO.getId())) {
            return SoulAdminResult.error("参数错误");
        }
        AppAuthDO appAuthDO = AppAuthTransfer.INSTANCE.mapToEntity(appAuthDTO);
        appAuthMapper.update(appAuthDO);
        List<AuthParamDTO> authParamDTOList = appAuthDTO.getAuthParamDTOList();
        if (CollectionUtils.isNotEmpty(authParamDTOList)) {
            authParamMapper.deleteByAuthId(appAuthDTO.getId());
            List<AuthParamDO> authParamDOList = authParamDTOList.stream()
                    .map(dto -> buildAuthParamDO(appAuthDTO.getId(), dto.getAppName(), dto.getAppParam()))
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
                    .map(authPathDTO -> buildAuthPathDO(authPathDTO.getPath(), appAuthDO.getId(), authPathDTO.getAppName()))
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
                    DataEventTypeEnum.UPDATE,
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
        AppAuthDO appAuthDO = AppAuthDO.buildAppAuthDO(appAuthDTO);
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
        AppAuthData data = new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(),
                appAuthDO.getEnabled(), null, null);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, eventType, Collections.singletonList(data)));

        return appAuthCount;
    }

    @Override
    public int delete(final List<String> ids) {
        int appAuthCount = 0;
        for (String id : ids) {
            appAuthCount += appAuthMapper.delete(id);
            authParamMapper.deleteByAuthId(id);
            authPathMapper.deleteByAuthId(id);
            // publish delete event of AppAuthData
            AppAuthDO appAuthDO = appAuthMapper.selectById(id);
            AppAuthData data = new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled(), null, null);
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
        PageParameter pageParameter = appAuthQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(),
                        appAuthMapper.countByQuery(appAuthQuery)),
                appAuthMapper.selectByQuery(appAuthQuery).stream()
                        .map(AppAuthTransfer.INSTANCE::mapToVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public List<AppAuthData> listAll() {
        return appAuthMapper.selectAll()
                .stream()
                .map(appAuthDO -> new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled(), null, null))
                .collect(Collectors.toList());
    }

    @Override
    public SoulAdminResult updateAppSecretByAppKey(final String appKey, final String appSecret) {
        return SoulAdminResult.success(appAuthMapper.updateAppSecretByAppKey(appKey, appSecret));
    }

    private AuthParamDO buildAuthParamDO(final String authId, final String appName, final String appParam) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        AuthParamDO authParamDO = new AuthParamDO();
        authParamDO.setId(UUIDUtils.getInstance().generateShortUuid());
        authParamDO.setAuthId(authId);
        authParamDO.setAppName(appName);
        authParamDO.setAppParam(appParam);
        authParamDO.setDateUpdated(currentTime);
        authParamDO.setDateCreated(currentTime);
        return authParamDO;
    }

    private AuthPathDO buildAuthPathDO(final String path, final String authId, final String appName) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        AuthPathDO authPathDO = new AuthPathDO();
        authPathDO.setId(UUIDUtils.getInstance().generateShortUuid());
        authPathDO.setAuthId(authId);
        authPathDO.setAppName(appName);
        authPathDO.setPath(path);
        authPathDO.setEnabled(true);
        authPathDO.setDateUpdated(currentTime);
        authPathDO.setDateCreated(currentTime);
        return authPathDO;
    }

    private AppAuthData buildByEntity(final AppAuthDO appAuthDO) {
        AppAuthData data = new AppAuthData();
        data.setAppKey(appAuthDO.getAppKey());
        data.setAppSecret(appAuthDO.getAppSecret());
        data.setEnabled(appAuthDO.getEnabled());
        List<AuthParamDO> authParamDOList = authParamMapper.findByAuthId(appAuthDO.getId());
        if (CollectionUtils.isNotEmpty(authParamDOList)) {
            data.setParamDataList(authParamDOList.stream()
                    .map(paramDO -> new AuthParamData(paramDO.getAppName(), paramDO.getAppParam()))
                    .collect(Collectors.toList()));
        }
        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthId(appAuthDO.getId());
        if (CollectionUtils.isNotEmpty(authPathDOList)) {
            data.setPathDataList(authPathDOList.stream()
                    .map(pathDO -> new AuthPathData(pathDO.getAppName(), pathDO.getPath(), pathDO.getEnabled()))
                    .collect(Collectors.toList()));
        }
        return data;
    }

}
