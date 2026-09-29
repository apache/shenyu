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
import com.google.common.collect.Sets;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.jpa.repository.AppAuthRepository;
import org.apache.shenyu.admin.jpa.repository.AuthParamRepository;
import org.apache.shenyu.admin.jpa.repository.AuthPathRepository;
import org.apache.shenyu.admin.listener.DataChangedEvent;
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
import org.apache.shenyu.admin.model.page.PageCondition;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
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
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.AppAuthService}.
 */
@Service
public class AppAuthServiceImpl implements AppAuthService {

    private static final Logger LOG = LoggerFactory.getLogger(AppAuthServiceImpl.class);

    private final AppAuthRepository appAuthRepository;

    private final ApplicationEventPublisher eventPublisher;

    private final AuthParamRepository authParamRepository;

    private final AuthPathRepository authPathRepository;

    public AppAuthServiceImpl(final AppAuthRepository appAuthRepository,
                              final ApplicationEventPublisher eventPublisher,
                              final AuthParamRepository authParamRepository,
                              final AuthPathRepository authPathRepository) {
        this.appAuthRepository = appAuthRepository;
        this.eventPublisher = eventPublisher;
        this.authParamRepository = authParamRepository;
        this.authPathRepository = authPathRepository;
    }

    @Override
    public boolean useJpaPage() {
        return true;
    }

    @Override
    public Page<AppAuthVO> jpaSearchByCondition(final PageCondition<AppAuthQuery> pageCondition) {
        PageRequest pageRequest = PageResultUtils.of(pageCondition);
        Page<AppAuthDO> page = appAuthRepository.pageByQuery(pageCondition.getCondition(), pageRequest);
        return new PageImpl<>(page.stream().map(AppAuthTransfer.INSTANCE::mapToVO).toList(), pageRequest, page.getTotalElements());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult applyCreate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isBlank(authApplyDTO.getAppName())
                || hasMissingPathsWhenOpen(authApplyDTO)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        AppAuthDO appAuthDO = AppAuthDO.create(authApplyDTO);
        appAuthRepository.save(appAuthDO);
        // save authParam
        AuthParamDO authParamDO = AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam());
        authParamRepository.save(authParamDO);

        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .namespaceId(appAuthDO.getNamespaceId())
                .paramDataList(Lists.newArrayList(new AuthParamData(authParamDO.getAppName(), authParamDO.getAppParam())))
                .build();

        // save authPath
        if (Boolean.TRUE.equals(appAuthDO.getOpen())) {
            List<AuthPathDO> collect = authApplyDTO.getPathList()
                    .stream()
                    .map(path -> AuthPathDO.create(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathRepository.saveAll(collect);
            data.setPathDataList(collect.stream().map(authPathDO ->
                            AuthPathData.builder().appName(authPathDO.getAppName()).path(authPathDO.getPath()).enabled(authPathDO.getEnabled()).build())
                    .collect(Collectors.toList()));
        }

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(data)));

        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult applyUpdate(final AuthApplyDTO authApplyDTO) {
        if (StringUtils.isAnyBlank(authApplyDTO.getAppKey(), authApplyDTO.getAppName())
                || hasMissingPathsWhenOpen(authApplyDTO)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.PARAMETER_ERROR);
        }
        AppAuthDO appAuthDO = appAuthRepository.findByAppKey(authApplyDTO.getAppKey()).orElse(null);
        if (Objects.isNull(appAuthDO)) {
            return ShenyuAdminResult.error(ShenyuResultMessage.APPKEY_NOT_EXIST_ERROR);
        }
        appAuthDO.setUserId(authApplyDTO.getUserId());
        appAuthDO.setPhone(authApplyDTO.getPhone());
        appAuthDO.setExtInfo(authApplyDTO.getExtInfo());
        if (Objects.nonNull(authApplyDTO.getOpen())) {
            appAuthDO.setOpen(authApplyDTO.getOpen());
        }
        appAuthRepository.save(appAuthDO);

        AuthParamDO authParamDO = authParamRepository.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName()).orElse(null);
        if (Objects.isNull(authParamDO)) {
            // save authParam
            authParamRepository.save(AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam()));
        }

        if (Boolean.TRUE.equals(authApplyDTO.getOpen())) {
            List<AuthPathDO> existList = authPathRepository.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
            if (CollectionUtils.isNotEmpty(existList)) {
                authPathRepository.deleteByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName());
            }
            List<AuthPathDO> collect = authApplyDTO.getPathList()
                    .stream()
                    .map(path -> AuthPathDO.create(path, appAuthDO.getId(), authApplyDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathRepository.saveAll(collect);
        }

        // publish create Event of APP_AUTH
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.CREATE,
                Collections.singletonList(buildByEntity(appAuthDO))));

        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult updateDetail(final AppAuthDTO appAuthDTO) {
        final AppAuthDO appAuthDO = AppAuthTransfer.INSTANCE.mapToEntity(appAuthDTO);
        final AppAuthDO merged = appAuthRepository.findByIdAndNamespaceId(appAuthDTO.getId(), appAuthDO.getNamespaceId())
                .map(persisted -> {
                    if (Objects.nonNull(appAuthDO.getAppKey())) {
                        persisted.setAppKey(appAuthDO.getAppKey());
                    }
                    if (Objects.nonNull(appAuthDO.getAppSecret())) {
                        persisted.setAppSecret(appAuthDO.getAppSecret());
                    }
                    if (Objects.nonNull(appAuthDO.getPhone())) {
                        persisted.setPhone(appAuthDO.getPhone());
                    }
                    if (Objects.nonNull(appAuthDO.getUserId())) {
                        persisted.setUserId(appAuthDO.getUserId());
                    }
                    if (Objects.nonNull(appAuthDO.getExtInfo())) {
                        persisted.setExtInfo(appAuthDO.getExtInfo());
                    }
                    if (Objects.nonNull(appAuthDO.getOpen())) {
                        persisted.setOpen(appAuthDO.getOpen());
                    }
                    if (Objects.nonNull(appAuthDO.getEnabled())) {
                        persisted.setEnabled(appAuthDO.getEnabled());
                    }
                    return appAuthRepository.save(persisted);
                })
                .orElse(appAuthDO);
        List<AuthParamDTO> authParamDTOList = appAuthDTO.getAuthParamList();
        if (CollectionUtils.isNotEmpty(authParamDTOList)) {
            authParamRepository.deleteByAuthId(appAuthDTO.getId());

            List<AuthParamDO> authParamDOList = authParamDTOList.stream()
                    .map(dto -> AuthParamDO.create(appAuthDTO.getId(), dto.getAppName(), dto.getAppParam()))
                    .collect(Collectors.toList());
            authParamRepository.saveAll(authParamDOList);
        }
        List<AuthPathDTO> authPathDTOList = appAuthDTO.getAuthPathList();
        if (CollectionUtils.isNotEmpty(authPathDTOList)) {
            List<AuthPathDO> oldAuthPathDOList = authPathRepository.findByAuthId(appAuthDTO.getId());
            String appName = oldAuthPathDOList.stream().findFirst()
                    .map(AuthPathDO::getAppName).orElse(StringUtils.EMPTY);

            authPathRepository.deleteByAuthId(appAuthDTO.getId());

            List<AuthPathDO> authPathDOList = authPathDTOList.stream()
                    .filter(Objects::nonNull)
                    .map(dto -> AuthPathDO.create(dto.getPath(), appAuthDTO.getId(), appName))
                    .collect(Collectors.toList());
            authPathRepository.saveAll(authPathDOList);
        }

        AppAuthData appAuthData = buildByEntity(merged);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(appAuthData)));
        return ShenyuAdminResult.success();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult updateDetailPath(final AuthPathWarpDTO authPathWarpDTO) {
        AppAuthDO appAuthDO = appAuthRepository.findById(authPathWarpDTO.getId()).orElse(null);
        if (Objects.isNull(appAuthDO)) {
            return ShenyuAdminResult.error(AdminConstants.ID_NOT_EXIST);
        }
        List<AuthPathDTO> authPathDTOList = authPathWarpDTO.getAuthPathDTOList();
        if (CollectionUtils.isNotEmpty(authPathDTOList)) {
            authPathRepository.deleteByAuthId(authPathWarpDTO.getId());

            List<AuthPathDO> collect = authPathDTOList.stream()
                    .filter(Objects::nonNull)
                    .map(authPathDTO -> AuthPathDO.create(authPathDTO.getPath(), appAuthDO.getId(), authPathDTO.getAppName()))
                    .collect(Collectors.toList());
            authPathRepository.saveAll(collect);
        }
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                DataEventTypeEnum.UPDATE,
                Lists.newArrayList(buildByEntity(appAuthDO))));
        return ShenyuAdminResult.success();
    }

    @Override
    public ShenyuAdminResult syncData() {
        List<AppAuthDO> appAuthDOList = appAuthRepository.findAll();
        return syncData(appAuthDOList);
    }

    public ShenyuAdminResult syncData(final List<AppAuthDO> appAuthDOList) {
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return ShenyuAdminResult.success();
        }

        List<String> idList = appAuthDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        Map<String, List<AuthParamData>> paramMap = this.prepareAuthParamData(idList);
        Map<String, List<AuthPathData>> pathMap = this.prepareAuthPathData(idList);

        Map<String, List<AppAuthData>> namespaceDataList = appAuthDOList.stream()
                .filter(Objects::nonNull)
                .map(appAuthDO -> {
                    String id = appAuthDO.getId();
                    return buildByEntityWithParamAndPath(appAuthDO, paramMap.get(id), pathMap.get(id));
                })
                .collect(Collectors.groupingBy(AppAuthData::getNamespaceId));
        namespaceDataList.values().forEach(dataList -> {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.REFRESH,
                    dataList));
        });

        return ShenyuAdminResult.success();
    }

    @Override
    public ShenyuAdminResult syncDataByNamespaceId(final String namespaceId) {
        List<AppAuthDO> appAuthDOList = appAuthRepository.findByNamespaceId(namespaceId);
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.REFRESH, Collections.emptyList(), namespaceId));
            return ShenyuAdminResult.success();
        }
        return syncData(appAuthDOList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final List<AppAuthDTO> authDataList) {
        if (CollectionUtils.isEmpty(authDataList)) {
            return ConfigImportResult.success();
        }
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        // exist appKey set
        Set<String> existAppKeySet = Optional.of(
                        this.appAuthRepository.findAll()
                                .stream()
                                .filter(Objects::nonNull)
                                .map(AppAuthDO::getAppKey)
                                .collect(Collectors.toSet()))
                .orElseGet(Sets::newHashSet);

        for (AppAuthDTO appAuth : authDataList) {
            String appKey = appAuth.getAppKey();
            if (existAppKeySet.contains(appKey)) {
                // already exists, just record fail info, and continue
                LOG.info("import auth data, appKey: {} already exists", appKey);
                errorMsgBuilder
                        .append(appKey)
                        .append(",");
                continue;
            }
            AppAuthDO appAuthDO = AppAuthTransfer.INSTANCE.mapToEntity(appAuth);
            // create
            String authId = UUIDUtils.getInstance().generateShortUuid();
            appAuthDO.setId(authId);
            appAuthRepository.save(appAuthDO);
            int inserted = 1;
            if (inserted > 0) {
                successCount++;
                // auth path
                List<AuthPathDTO> authPathDTOList = appAuth.getAuthPathList();
                if (CollectionUtils.isNotEmpty(authPathDTOList)) {
                    List<AuthPathDO> authPathDOS = authPathDTOList
                            .stream()
                            .map(param -> AuthPathDO.create(param.getPath(), authId, param.getAppName()))
                            .collect(Collectors.toList());
                    authPathRepository.saveAll(authPathDOS);
                }

                // auth param
                List<AuthParamDTO> authParamVOList = appAuth.getAuthParamList();
                if (CollectionUtils.isNotEmpty(authParamVOList)) {
                    List<AuthParamDO> authParamDOS = authParamVOList
                            .stream()
                            .map(param -> AuthParamDO.create(authId, param.getAppName(), param.getAppParam()))
                            .collect(Collectors.toList());
                    authParamRepository.saveAll(authParamDOS);
                }
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            return ConfigImportResult.fail(successCount, "import fail appKey: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ConfigImportResult importData(final String namespace, final List<AppAuthDTO> authDataList) {
        if (CollectionUtils.isEmpty(authDataList)) {
            return ConfigImportResult.success();
        }
        StringBuilder errorMsgBuilder = new StringBuilder();
        int successCount = 0;
        // exist appKey set
        Set<String> existAppKeySet = Optional.of(
                        this.appAuthRepository.findByNamespaceId(namespace)
                                .stream()
                                .filter(Objects::nonNull)
                                .map(AppAuthDO::getAppKey)
                                .collect(Collectors.toSet()))
                .orElseGet(Sets::newHashSet);
        
        for (AppAuthDTO appAuth : authDataList) {
            String appKey = appAuth.getAppKey();
            if (existAppKeySet.contains(appKey)) {
                // already exists, just record fail info, and continue
                LOG.info("import auth data, appKey: {} already exists", appKey);
                errorMsgBuilder
                        .append(appKey)
                        .append(",");
                continue;
            }
            AppAuthDO appAuthDO = AppAuthTransfer.INSTANCE.mapToEntity(appAuth);
            // create
            String authId = UUIDUtils.getInstance().generateShortUuid();
            appAuthDO.setId(authId);
            appAuthDO.setNamespaceId(namespace);
            appAuthRepository.save(appAuthDO);
            int inserted = 1;
            if (inserted > 0) {
                successCount++;
                // auth path
                List<AuthPathDTO> authPathDTOList = appAuth.getAuthPathList();
                if (CollectionUtils.isNotEmpty(authPathDTOList)) {
                    List<AuthPathDO> authPathDOS = authPathDTOList
                            .stream()
                            .map(param -> AuthPathDO.create(param.getPath(), authId, param.getAppName()))
                            .collect(Collectors.toList());
                    authPathRepository.saveAll(authPathDOS);
                }
                
                // auth param
                List<AuthParamDTO> authParamVOList = appAuth.getAuthParamList();
                if (CollectionUtils.isNotEmpty(authParamVOList)) {
                    List<AuthParamDO> authParamDOS = authParamVOList
                            .stream()
                            .map(param -> AuthParamDO.create(authId, param.getAppName(), param.getAppParam()))
                            .collect(Collectors.toList());
                    authParamRepository.saveAll(authParamDOS);
                }
            }
        }
        if (StringUtils.isNotEmpty(errorMsgBuilder)) {
            return ConfigImportResult.fail(successCount, "import fail appKey: " + errorMsgBuilder);
        }
        return ConfigImportResult.success(successCount);
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
            appAuthRepository.save(appAuthDO);
            appAuthCount = 1;
            eventType = DataEventTypeEnum.CREATE;
        } else {
            appAuthCount = appAuthRepository.findByIdAndNamespaceId(appAuthDO.getId(), appAuthDO.getNamespaceId())
                    .map(persisted -> {
                        if (Objects.nonNull(appAuthDO.getAppKey())) {
                            persisted.setAppKey(appAuthDO.getAppKey());
                        }
                        if (Objects.nonNull(appAuthDO.getAppSecret())) {
                            persisted.setAppSecret(appAuthDO.getAppSecret());
                        }
                        if (Objects.nonNull(appAuthDO.getPhone())) {
                            persisted.setPhone(appAuthDO.getPhone());
                        }
                        if (Objects.nonNull(appAuthDO.getUserId())) {
                            persisted.setUserId(appAuthDO.getUserId());
                        }
                        if (Objects.nonNull(appAuthDO.getExtInfo())) {
                            persisted.setExtInfo(appAuthDO.getExtInfo());
                        }
                        if (Objects.nonNull(appAuthDO.getOpen())) {
                            persisted.setOpen(appAuthDO.getOpen());
                        }
                        if (Objects.nonNull(appAuthDO.getEnabled())) {
                            persisted.setEnabled(appAuthDO.getEnabled());
                        }
                        appAuthRepository.save(persisted);
                        return 1;
                    })
                    .orElse(0);
            eventType = DataEventTypeEnum.UPDATE;
        }
        if (appAuthCount == 0) {
            return 0;
        }
        // publish AppAuthData's event
        AppAuthData data = AppAuthData.builder()
                .namespaceId(appAuthDO.getNamespaceId())
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
        List<AppAuthDO> appAuthList = appAuthRepository.findAllById(ids);
        if (CollectionUtils.isEmpty(appAuthList)) {
            return 0;
        }
        int affectCount = appAuthRepository.deleteByIds(ids);
        if (affectCount <= 0) {
            return affectCount;
        }
        authParamRepository.deleteByAuthIds(ids);
        authPathRepository.deleteByAuthIds(ids);

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
        List<AppAuthDO> appAuthDOList = appAuthRepository.findAllById(distinctIds);
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

        appAuthRepository.updateEnableBatch(distinctIds, enabled);

        // publish change event.
        if (CollectionUtils.isNotEmpty(authDataList)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.UPDATE,
                    authDataList));
        }
        return StringUtils.EMPTY;
    }

    @Override
    public String opened(final List<String> ids, final Boolean enabled) {
        List<String> distinctIds = ids.stream().distinct().collect(Collectors.toList());
        List<AppAuthDO> appAuthDOList = appAuthRepository.findAllById(distinctIds);
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return AdminConstants.ID_NOT_EXIST;
        }

        Map<String, List<AuthParamData>> paramMap = this.prepareAuthParamData(distinctIds);
        Map<String, List<AuthPathData>> pathMap = this.prepareAuthPathData(distinctIds);

        List<AppAuthData> authDataList = appAuthDOList.stream().map(appAuthDO -> {
            String id = appAuthDO.getId();
            appAuthDO.setOpen(enabled);
            return this.buildByEntityWithParamAndPath(appAuthDO, paramMap.get(id), pathMap.get(id));
        }).collect(Collectors.toList());

        appAuthRepository.updateOpenBatch(distinctIds, enabled);

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
        AppAuthVO appAuthVO = AppAuthTransfer.INSTANCE.mapToVO(appAuthRepository.findById(id).orElse(null));
        List<AuthParamDO> authParamDOList = authParamRepository.findByAuthId(id);
        if (CollectionUtils.isNotEmpty(authParamDOList)) {
            appAuthVO.setAuthParamList(authParamDOList.stream().map(authParamDO -> {
                AuthParamVO vo = new AuthParamVO();
                vo.setAppName(authParamDO.getAppName());
                vo.setAppParam(authParamDO.getAppParam());
                return vo;
            }).collect(Collectors.toList()));
        }
        appAuthVO.setAuthPathList(detailPath(id));
        return appAuthVO;
    }

    @Override
    public List<AuthPathVO> detailPath(final String authId) {
        List<AuthPathDO> authPathDOList = authPathRepository.findByAuthId(authId);
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
        Page<AppAuthDO> page = appAuthRepository.pageByQuery(appAuthQuery, PageResultUtils.of(appAuthQuery.getPageParameter()));
        return PageResultUtils.result(appAuthQuery.getPageParameter(), page, AppAuthTransfer.INSTANCE::mapToVO);
    }

    @Override
    public List<AppAuthData> listAll() {
        return buildSyncData(appAuthRepository.findAll());
    }

    @Override
    public List<AppAuthData> listAllByNamespaceId(final String namespaceId) {
        return buildSyncData(appAuthRepository.findByNamespaceId(namespaceId));
    }

    private List<AppAuthData> buildSyncData(final List<AppAuthDO> appAuthDOList) {
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
    public List<AppAuthVO> listAllData() {
        List<AppAuthDO> appAuthDOList = appAuthRepository.findAll();
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return new ArrayList<>();
        }

        List<String> idList = appAuthDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        Map<String, List<AuthParamVO>> paramMap = this.prepareAuthParamVO(idList);
        Map<String, List<AuthPathVO>> pathMap = this.prepareAuthPathVO(idList);

        return appAuthDOList.stream().map(data -> {
                AppAuthVO vo = AppAuthTransfer.INSTANCE.mapToVO(data);
                vo.setAuthParamList(paramMap.get(vo.getId()));
                vo.setAuthPathList(pathMap.get(vo.getId()));
                return vo;
            }
        ).collect(Collectors.toList());
    }
    
    @Override
    public List<AppAuthVO> listAllDataByNamespace(final String namespace) {
        
        List<AppAuthDO> appAuthDOList = appAuthRepository.findByNamespaceId(namespace);
        if (CollectionUtils.isEmpty(appAuthDOList)) {
            return new ArrayList<>();
        }
        
        List<String> idList = appAuthDOList.stream().map(BaseDO::getId).collect(Collectors.toList());
        Map<String, List<AuthParamVO>> paramMap = this.prepareAuthParamVO(idList);
        Map<String, List<AuthPathVO>> pathMap = this.prepareAuthPathVO(idList);
        
        return appAuthDOList.stream().map(data -> {
                AppAuthVO vo = AppAuthTransfer.INSTANCE.mapToVO(data);
                vo.setAuthParamList(paramMap.get(vo.getId()));
                vo.setAuthPathList(pathMap.get(vo.getId()));
                return vo;
            }
        ).collect(Collectors.toList());
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ShenyuAdminResult updateAppSecretByAppKey(final String appKey, final String appSecret) {
        int count = appAuthRepository.updateAppSecretByAppKey(appKey, appSecret);
        if (count > 0) {
            AppAuthDO appAuthDO = appAuthRepository.findByAppKey(appKey).orElse(null);
            if (Objects.nonNull(appAuthDO)) {
                AppAuthData appAuthData = buildByEntity(appAuthDO);
                eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                        DataEventTypeEnum.UPDATE,
                        Lists.newArrayList(appAuthData)));
            }
        }
        return ShenyuAdminResult.success(count);
    }

    @Override
    public AppAuthDO findByAppKey(final String appKey) {
        return appAuthRepository.findByAppKey(appKey).orElse(null);
    }

    private AppAuthData buildByEntity(final AppAuthDO appAuthDO) {
        AppAuthData data = AppAuthData.builder()
                .appKey(appAuthDO.getAppKey())
                .appSecret(appAuthDO.getAppSecret())
                .open(appAuthDO.getOpen())
                .enabled(appAuthDO.getEnabled())
                .namespaceId(appAuthDO.getNamespaceId())
                .build();
        List<AuthParamDO> authParamDOList = authParamRepository.findByAuthId(appAuthDO.getId());
        if (CollectionUtils.isNotEmpty(authParamDOList)) {
            data.setParamDataList(
                    authParamDOList.stream()
                            .map(paramDO -> new AuthParamData(paramDO.getAppName(), paramDO.getAppParam()))
                            .collect(Collectors.toList())
            );
        }
        List<AuthPathDO> authPathDOList = authPathRepository.findByAuthId(appAuthDO.getId());
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
                .namespaceId(appAuthDO.getNamespaceId())
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
     *
     * @param authIds auth id
     * @return a map consist of param info
     */
    private Map<String, List<AuthParamData>> prepareAuthParamData(final List<String> authIds) {

        List<AuthParamDO> authPathDOList = authParamRepository.findByAuthIdIn(authIds);

        return Optional.ofNullable(authPathDOList).orElseGet(ArrayList::new)
                .stream().collect(Collectors.toMap(AuthParamDO::getAuthId, data -> {
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
     *
     * @param authIds auth id
     * @return a map consist of path info
     */
    private Map<String, List<AuthPathData>> prepareAuthPathData(final List<String> authIds) {

        List<AuthPathDO> authPathDOList = authPathRepository.findByAuthIdIn(authIds);
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

    /**
     * prepare the Map with authIds.
     *
     * @param authIds auth id
     * @return a map consist of param vo info
     */
    private Map<String, List<AuthParamVO>> prepareAuthParamVO(final List<String> authIds) {

        List<AuthParamDO> authPathDOList = authParamRepository.findByAuthIdIn(authIds);

        return Optional.ofNullable(authPathDOList).orElseGet(ArrayList::new)
                .stream().collect(Collectors.toMap(AuthParamDO::getAuthId,
                        data -> {
                            List<AuthParamVO> dataList = new ArrayList<>();
                            AuthParamVO authParamVO = AppAuthTransfer.INSTANCE.mapToVO(data);
                            dataList.add(authParamVO);
                            return dataList;
                        }, (List<AuthParamVO> dataList1, List<AuthParamVO> dataList2) -> {
                            dataList1.addAll(dataList2);
                            return dataList1;
                        }));
    }

    /**
     * prepare the Map with authIds.
     *
     * @param authIds auth id
     * @return a map consist of path vo info
     */
    private Map<String, List<AuthPathVO>> prepareAuthPathVO(final List<String> authIds) {

        List<AuthPathDO> authPathDOList = authPathRepository.findByAuthIdIn(authIds);
        return Optional.ofNullable(authPathDOList).orElseGet(ArrayList::new)
                .stream().collect(Collectors.toMap(AuthPathDO::getAuthId,
                        data -> {
                            List<AuthPathVO> dataList = new ArrayList<>();
                            AuthPathVO authPathVO = AppAuthTransfer.INSTANCE.mapToVO(data);
                            dataList.add(authPathVO);
                            return dataList;
                        }, (List<AuthPathVO> dataList1, List<AuthPathVO> dataList2) -> {
                            dataList1.addAll(dataList2);
                            return dataList1;
                        }));
    }

    /**
     * check whether the path list is empty when open.
     *
     * @param authApplyDTO auth apply dto
     * @return true if the path list is empty when open
     */
    private boolean hasMissingPathsWhenOpen(final AuthApplyDTO authApplyDTO) {
        return Boolean.TRUE.equals(authApplyDTO.getOpen()) && CollectionUtils.isEmpty(authApplyDTO.getPathList());
    }

}
