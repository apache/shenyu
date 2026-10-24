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

package org.apache.shenyu.admin.service;

import com.google.common.collect.Lists;
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
import org.apache.shenyu.admin.model.dto.BatchCommonDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.AuthPathVO;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Test cases for AppAuthService.
 */
@ExtendWith(MockitoExtension.class)
public final class AppAuthServiceTest {

    @InjectMocks
    private AppAuthServiceImpl appAuthService;

    @Mock
    private AppAuthMapper appAuthMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private AuthParamMapper authParamMapper;

    @Mock
    private AuthPathMapper authPathMapper;

    private final AppAuthDO appAuthDO = buildAppAuthDO();

    @Test
    public void testApplyCreate() {
        testApplyCreateParameterError();
        testApplyCreateSuccess();
    }

    @Test
    public void testApplyUpdate() {
        testApplyUpdateParameterError();
        testApplyUpdateAppKeyNotExist();
        testApplyUpdateSuccess();
    }

    @Test
    public void testCreateOrUpdatePropagatesNamespace() {
        AppAuthDTO dto = buildAppAuthDTO("auth-id");
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        given(appAuthMapper.updateSelective(any())).willReturn(1);
        assertEquals(1, appAuthService.createOrUpdate(dto));
        verify(appAuthMapper).updateSelective(argThat(auth -> SYS_DEFAULT_NAMESPACE_ID.equals(auth.getNamespaceId())));
        verify(eventPublisher).publishEvent(any());
    }

    @Test
    public void testRejectedNamespaceUpdateDoesNotPublish() {
        AppAuthDTO dto = buildAppAuthDTO("auth-id");
        dto.setNamespaceId("other-namespace");
        given(appAuthMapper.updateSelective(any())).willReturn(0);
        assertEquals(0, appAuthService.createOrUpdate(dto));
        verifyNoInteractions(eventPublisher);
    }

    @Test
    public void testApplyUpdatePreservesOpenWhenOmitted() {
        final AuthApplyDTO authApplyDTO = buildAuthApplyDTO();
        authApplyDTO.setOpen(null);
        authApplyDTO.setPathList(null);
        appAuthDO.setOpen(true);
        given(appAuthMapper.findByAppKey(appAuthDO.getAppKey())).willReturn(appAuthDO);
        given(authParamMapper.findByAuthIdAndAppName(appAuthDO.getId(), authApplyDTO.getAppName()))
                .willReturn(AuthParamDO.create(appAuthDO.getId(), authApplyDTO.getAppName(), authApplyDTO.getAppParam()));
        given(authPathMapper.findByAuthId(appAuthDO.getId())).willReturn(Collections.emptyList());
        given(authParamMapper.findByAuthId(appAuthDO.getId())).willReturn(Collections.emptyList());

        ShenyuAdminResult result = appAuthService.applyUpdate(authApplyDTO);

        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, result.getMessage());
        verify(appAuthMapper).updateSelective(argThat(updated -> Boolean.TRUE.equals(updated.getOpen())));
        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        AppAuthData publishedData = (AppAuthData) eventCaptor.getValue().getSource().get(0);
        assertEquals(Boolean.TRUE, publishedData.getOpen());
    }

    @Test
    public void testUpdateDetail() {
        AppAuthDTO appAuthDTO = buildAppAuthDTO(UUIDUtils.getInstance().generateShortUuid());
        List<AuthParamDTO> authParamDTOList = Collections.singletonList(buildAuthParamDTO());
        appAuthDTO.setAuthParamList(authParamDTOList);
        ShenyuAdminResult successResult = this.appAuthService.updateDetail(appAuthDTO);
        assertEquals(CommonErrorCode.SUCCESSFUL, successResult.getCode().intValue());
        verify(appAuthMapper).updateSelective(any(AppAuthDO.class));
        verify(appAuthMapper, never()).update(any(AppAuthDO.class));
    }

    @Test
    public void testUpdateDetailPath() {
        AuthPathWarpDTO authPathWarpDTO = new AuthPathWarpDTO();
        List<AuthPathDTO> authPathDTOList = Collections.singletonList(buildAuthPathDTO());
        authPathWarpDTO.setId(UUIDUtils.getInstance().generateShortUuid());
        authPathWarpDTO.setAuthPathDTOList(authPathDTOList);
        ShenyuAdminResult idNotExistResult = this.appAuthService.updateDetailPath(authPathWarpDTO);
        assertEquals(AdminConstants.ID_NOT_EXIST, idNotExistResult.getMessage());

        given(appAuthMapper.selectById(eq(authPathWarpDTO.getId()))).willReturn(appAuthDO);
        ShenyuAdminResult successResult = this.appAuthService.updateDetailPath(authPathWarpDTO);
        assertEquals(CommonErrorCode.SUCCESSFUL, successResult.getCode().intValue());
    }

    @Test
    public void testCreateOrUpdate() {
        AppAuthDTO insertAppAuthDTO = buildAppAuthDTO();
        given(this.appAuthMapper.insertSelective(any())).willReturn(1);
        assertThat(this.appAuthService.createOrUpdate(insertAppAuthDTO), greaterThan(0));

        AppAuthDTO updateAppAuthDTO = buildAppAuthDTO(UUIDUtils.getInstance().generateShortUuid());
        given(this.appAuthMapper.updateSelective(any())).willReturn(1);
        assertThat(this.appAuthService.createOrUpdate(updateAppAuthDTO), greaterThan(0));
    }

    @ParameterizedTest
    @ValueSource(strings = {"ns-A", SYS_DEFAULT_NAMESPACE_ID})
    public void testDeletePreservesNamespace(final String namespaceId) {
        appAuthDO.setNamespaceId(namespaceId);
        List<String> ids = Collections.singletonList(appAuthDO.getId());
        given(appAuthMapper.selectByIds(ids)).willReturn(Collections.singletonList(appAuthDO));
        given(appAuthMapper.deleteByIds(ids)).willReturn(1);

        assertEquals(1, appAuthService.delete(ids));

        verify(authParamMapper).deleteByAuthIds(ids);
        verify(authPathMapper).deleteByAuthIds(ids);
        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        assertDeleteEvent(eventCaptor.getValue(), namespaceId, appAuthDO.getAppKey());
    }

    @Test
    public void testDeletePublishesOneEventPerNamespace() {
        AppAuthDO first = buildAppAuthDO("auth-A1", "ns-A");
        AppAuthDO second = buildAppAuthDO("auth-B", "ns-B");
        AppAuthDO third = buildAppAuthDO("auth-A2", "ns-A");
        AppAuthDO defaultAuth = buildAppAuthDO("auth-default", SYS_DEFAULT_NAMESPACE_ID);
        List<AppAuthDO> authList = List.of(first, second, third, defaultAuth);
        List<String> ids = authList.stream().map(AppAuthDO::getId).collect(Collectors.toList());
        given(appAuthMapper.selectByIds(ids)).willReturn(authList);
        given(appAuthMapper.deleteByIds(ids)).willReturn(authList.size());

        assertEquals(authList.size(), appAuthService.delete(ids));

        verify(authParamMapper).deleteByAuthIds(ids);
        verify(authPathMapper).deleteByAuthIds(ids);
        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(3)).publishEvent(eventCaptor.capture());
        Map<String, DataChangedEvent> events = eventCaptor.getAllValues().stream()
                .collect(Collectors.toMap(DataChangedEvent::getNamespaceId, event -> event));
        assertEquals(Set.of("ns-A", "ns-B", SYS_DEFAULT_NAMESPACE_ID), events.keySet());
        assertDeleteEvent(events.get("ns-A"), "ns-A", first.getAppKey(), third.getAppKey());
        assertDeleteEvent(events.get("ns-B"), "ns-B", second.getAppKey());
        assertDeleteEvent(events.get(SYS_DEFAULT_NAMESPACE_ID), SYS_DEFAULT_NAMESPACE_ID, defaultAuth.getAppKey());
    }

    @Test
    public void testDeleteFallsBackToDefaultNamespaceWhenMissing() {
        AppAuthDO legacyAuth = buildAppAuthDO("auth-legacy", null);
        AppAuthDO defaultAuth = buildAppAuthDO("auth-default", SYS_DEFAULT_NAMESPACE_ID);
        List<AppAuthDO> authList = List.of(legacyAuth, defaultAuth);
        List<String> ids = authList.stream().map(AppAuthDO::getId).collect(Collectors.toList());
        given(appAuthMapper.selectByIds(ids)).willReturn(authList);
        given(appAuthMapper.deleteByIds(ids)).willReturn(authList.size());

        assertEquals(authList.size(), appAuthService.delete(ids));

        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        assertDeleteEvent(eventCaptor.getValue(), SYS_DEFAULT_NAMESPACE_ID, legacyAuth.getAppKey(), defaultAuth.getAppKey());
    }

    @ParameterizedTest
    @NullAndEmptySource
    public void testDeleteWithoutIdsDoesNotPublish(final List<String> ids) {
        assertEquals(0, appAuthService.delete(ids));

        verifyNoInteractions(appAuthMapper, authParamMapper, authPathMapper, eventPublisher);
    }

    @Test
    public void testDeleteMissingRecordsDoesNotPublish() {
        List<String> ids = Collections.singletonList(appAuthDO.getId());
        given(appAuthMapper.selectByIds(ids)).willReturn(Collections.emptyList());

        assertEquals(0, appAuthService.delete(ids));

        verify(appAuthMapper, never()).deleteByIds(any());
        verifyNoInteractions(authParamMapper, authPathMapper, eventPublisher);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, -1})
    public void testDeleteWithoutAffectedRowsDoesNotPublish(final int affectCount) {
        List<String> ids = Collections.singletonList(appAuthDO.getId());
        given(appAuthMapper.selectByIds(ids)).willReturn(Collections.singletonList(appAuthDO));
        given(appAuthMapper.deleteByIds(ids)).willReturn(affectCount);

        assertEquals(affectCount, appAuthService.delete(ids));

        verifyNoInteractions(authParamMapper, authPathMapper, eventPublisher);
    }

    private void assertDeleteEvent(final DataChangedEvent event, final String namespaceId, final String... appKeys) {
        assertEquals(ConfigGroupEnum.APP_AUTH, event.getGroupKey());
        assertEquals(DataEventTypeEnum.DELETE, ReflectionTestUtils.invokeMethod(event, "getEventType"));
        assertEquals(namespaceId, event.getNamespaceId());
        assertEquals(appKeys.length, event.getSource().size());
        List<AppAuthData> authData = event.getSource().stream().map(AppAuthData.class::cast).collect(Collectors.toList());
        authData.forEach(data -> assertEquals(namespaceId, data.getNamespaceId()));
        assertEquals(Set.of(appKeys), authData.stream().map(AppAuthData::getAppKey).collect(Collectors.toSet()));
    }

    @Test
    public void testOpened() {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(Boolean.TRUE);
        batchCommonDTO.setIds(Collections.singletonList(appAuthDO.getId()));
        assertEquals(AdminConstants.ID_NOT_EXIST, this.appAuthService.opened(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()));

        given(this.appAuthMapper.selectById(appAuthDO.getId())).willReturn(appAuthDO);
        given(this.appAuthMapper.selectByIds(Collections.singletonList(appAuthDO.getId()))).willReturn(Collections.singletonList(appAuthDO));
        assertEquals(StringUtils.EMPTY, this.appAuthService.opened(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()));
        AppAuthVO appAuthVO = this.appAuthService.findById(appAuthDO.getId());
        assertEquals(Boolean.TRUE, appAuthVO.getOpen());
    }

    @Test
    public void testEnabled() {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(Boolean.TRUE);
        batchCommonDTO.setIds(Collections.singletonList(appAuthDO.getId()));
        assertEquals(AdminConstants.ID_NOT_EXIST, this.appAuthService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()));

        given(this.appAuthMapper.selectById(appAuthDO.getId())).willReturn(appAuthDO);
        given(this.appAuthMapper.selectByIds(Collections.singletonList(appAuthDO.getId()))).willReturn(Collections.singletonList(appAuthDO));
        assertEquals(StringUtils.EMPTY, this.appAuthService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()));
        AppAuthVO appAuthVO = this.appAuthService.findById(appAuthDO.getId());
        assertEquals(Boolean.TRUE, appAuthVO.getEnabled());
    }

    @Test
    public void testFindById() {
        String authId = UUIDUtils.getInstance().generateShortUuid();
        String appName = "testAppName";
        String appParam = "{\"type\": \"test\"}";
        AuthParamDO authParamDO = AuthParamDO.create(authId, appName, appParam);
        List<AuthParamDO> authParamDOList = Collections.singletonList(authParamDO);
        given(this.authParamMapper.findByAuthId(eq(appAuthDO.getId()))).willReturn(authParamDOList);
        given(this.appAuthMapper.selectById(eq(appAuthDO.getId()))).willReturn(appAuthDO);
        AppAuthVO appAuthVO = this.appAuthService.findById(appAuthDO.getId());
        assertNotNull(appAuthVO);
        assertEquals(appAuthDO.getId(), appAuthVO.getId());
        assertNotNull(appAuthVO.getAuthParamList());
    }

    @Test
    public void testDetailPath() {
        AuthPathDO authPathDO = new AuthPathDO();
        String authPathDoId = UUIDUtils.getInstance().generateShortUuid();
        String authPathDOAuthId = UUIDUtils.getInstance().generateShortUuid();
        List<AuthPathVO> authPathVOListEmpty = this.appAuthService.detailPath(authPathDOAuthId);
        assertEquals(0, authPathVOListEmpty.size());
        authPathDO.setId(authPathDoId);
        authPathDO.setAuthId(authPathDOAuthId);
        given(this.authPathMapper.findByAuthId(eq(authPathDOAuthId))).willReturn(Collections.singletonList(authPathDO));
        List<AuthPathVO> authPathVOList = this.appAuthService.detailPath(authPathDOAuthId);
        assertEquals(1, authPathVOList.size());
        assertEquals(authPathDoId, authPathVOList.get(0).getId());
    }

    @Test
    public void testListByPage() {
        given(this.appAuthMapper.selectByQuery(any())).willReturn(Collections.singletonList(appAuthDO));
        AppAuthQuery appAuthQuery = new AppAuthQuery();
        appAuthQuery.setPageParameter(new PageParameter());
        CommonPager<AppAuthVO> appAuthVOCommonPager = this.appAuthService.listByPage(appAuthQuery);
        assertEquals(1, appAuthVOCommonPager.getDataList().size());
    }

    @Test
    public void testUpdateAppSecretByAppKey() {
        String newAppSecret = SignUtils.generateKey();
        appAuthDO.setAppSecret(newAppSecret);
        given(this.appAuthMapper.updateAppSecretByAppKey(appAuthDO.getAppKey(), appAuthDO.getAppSecret())).willReturn(1);
        ShenyuAdminResult result = this.appAuthService.updateAppSecretByAppKey(appAuthDO.getAppKey(), appAuthDO.getAppSecret());
        assertThat((int) result.getData(), greaterThan(0));
    }

    @Test
    public void testListAll() {
        given(this.appAuthMapper.selectAll()).willReturn(Collections.singletonList(appAuthDO));
        List<AppAuthData> appAuthDataList = this.appAuthService.listAll();
        assertEquals(1, appAuthDataList.size());
        assertEquals(appAuthDO.getAppKey(), appAuthDataList.get(0).getAppKey());
    }

    @Test
    public void testListAllData() {
        given(this.appAuthMapper.selectAll()).willReturn(Collections.singletonList(appAuthDO));
        List<AppAuthVO> appAuthDataList = this.appAuthService.listAllData();
        assertEquals(1, appAuthDataList.size());
        assertEquals(appAuthDO.getAppKey(), appAuthDataList.get(0).getAppKey());
    }

    @Test
    public void testImportData() {
        List<AppAuthDO> list = Lists.newArrayList(new AppAuthDO());
        when(appAuthMapper.selectAll()).thenReturn(list);

        AppAuthDTO appAuthDTO = buildAppAuthDTO();
        final List<AppAuthDTO> appAuthDTOList = Collections.singletonList(appAuthDTO);
        given(this.appAuthMapper.insertSelective(any())).willReturn(1);

        ConfigImportResult configImportResult = this.appAuthService.importData(appAuthDTOList);

        assertNotNull(configImportResult);
        Assertions.assertEquals(configImportResult.getSuccessCount(), appAuthDTOList.size());
    }

    @Test
    public void testSyncData() {
        AppAuthDO authDO = new AppAuthDO();
        authDO.setNamespaceId("test");
        ArrayList<AppAuthDO> all = Lists.newArrayList(authDO);
        when(appAuthMapper.selectAll())
                .thenReturn(null)
                .thenReturn(Lists.newArrayList())
                .thenReturn(all);
        doNothing().when(eventPublisher).publishEvent(any());
        for (int i = 0; i < 3; i++) {
            appAuthService.syncData();
        }
        verify(eventPublisher, times(1)).publishEvent(any());
    }

    @Test
    public void testSyncEmptyDataByNamespaceId() {
        String namespaceId = "namespace-id";
        when(appAuthMapper.selectAllByNamespaceId(namespaceId)).thenReturn(Collections.emptyList());

        appAuthService.syncDataByNamespaceId(namespaceId);

        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        DataChangedEvent event = eventCaptor.getValue();
        assertEquals(ConfigGroupEnum.APP_AUTH, event.getGroupKey());
        assertEquals(namespaceId, event.getNamespaceId());
        assertEquals(Collections.emptyList(), event.getSource());
    }

    private void testApplyCreateParameterError() {
        testApplyCreateAppNameBlank();
        testApplyCreateMissingPathsWhenOpen();
    }

    private void testApplyCreateAppNameBlank() {
        AuthApplyDTO authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppName("");
        ShenyuAdminResult parameterErrorResult = this.appAuthService.applyCreate(authApplyDTO);
        assertEquals(ShenyuResultMessage.PARAMETER_ERROR, parameterErrorResult.getMessage());
    }

    private void testApplyCreateMissingPathsWhenOpen() {
        AuthApplyDTO authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppName("appName");
        authApplyDTO.setOpen(true);
        authApplyDTO.setPathList(Collections.emptyList());
        ShenyuAdminResult parameterErrorResult = this.appAuthService.applyCreate(authApplyDTO);
        assertEquals(ShenyuResultMessage.PARAMETER_ERROR, parameterErrorResult.getMessage());
    }

    private void testApplyCreateSuccess() {
        AuthApplyDTO newAuthApplyDTO = buildAuthApplyDTO();
        given(this.appAuthMapper.insert(any())).willReturn(1);
        given(this.authParamMapper.save(any())).willReturn(1);
        ShenyuAdminResult successResult = this.appAuthService.applyCreate(newAuthApplyDTO);
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, successResult.getMessage());
    }

    private void testApplyUpdateParameterError() {
        testApplyUpdateAppNameBlank();
        testApplyUpdateMissingPathsWhenOpen();
    }

    private void testApplyUpdateAppNameBlank() {
        AuthApplyDTO authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppName("");
        ShenyuAdminResult parameterErrorResult = this.appAuthService.applyUpdate(authApplyDTO);
        assertEquals(ShenyuResultMessage.PARAMETER_ERROR, parameterErrorResult.getMessage());
    }

    private void testApplyUpdateMissingPathsWhenOpen() {
        AuthApplyDTO authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppName("appName");
        authApplyDTO.setOpen(true);
        authApplyDTO.setPathList(Collections.emptyList());
        ShenyuAdminResult parameterErrorResult = this.appAuthService.applyUpdate(authApplyDTO);
        assertEquals(ShenyuResultMessage.PARAMETER_ERROR, parameterErrorResult.getMessage());
    }

    private void testApplyUpdateAppKeyNotExist() {
        ShenyuAdminResult appKeyNotExistResult = this.appAuthService.applyUpdate(buildAuthApplyDTO());
        assertEquals(ShenyuResultMessage.APPKEY_NOT_EXIST_ERROR, appKeyNotExistResult.getMessage());
    }

    private void testApplyUpdateSuccess() {
        final AuthApplyDTO authApplyDTO = buildAuthApplyDTO();
        appAuthDO.setOpen(false);
        AuthPathDO authPathDO = new AuthPathDO();
        String authPathDoId = UUIDUtils.getInstance().generateShortUuid();
        String authPathDOAuthId = UUIDUtils.getInstance().generateShortUuid();
        authPathDO.setId(authPathDoId);
        authPathDO.setAuthId(authPathDOAuthId);
        String appName = "testAppName";
        String appParam = "{\"type\": \"test\"}";
        AuthParamDO authParamDO = AuthParamDO.create(appAuthDO.getId(), appName, appParam);
        List<AuthParamDO> authParamDOList = Collections.singletonList(authParamDO);
        given(this.authPathMapper.findByAuthIdAndAppName(
                eq(appAuthDO.getId()), eq(authApplyDTO.getAppName()))).willReturn(Collections.singletonList(authPathDO));
        given(this.appAuthMapper.findByAppKey(appAuthDO.getAppKey())).willReturn(appAuthDO);
        given(authPathMapper.findByAuthId(eq(appAuthDO.getId()))).willReturn(Collections.singletonList(authPathDO));
        given(authParamMapper.findByAuthId(eq(appAuthDO.getId()))).willReturn(authParamDOList);
        ShenyuAdminResult successResult = this.appAuthService.applyUpdate(authApplyDTO);
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, successResult.getMessage());
        verify(appAuthMapper).updateSelective(argThat(updated -> authApplyDTO.getUserId().equals(updated.getUserId())
                && authApplyDTO.getPhone().equals(updated.getPhone())
                && authApplyDTO.getExtInfo().equals(updated.getExtInfo())
                && Boolean.TRUE.equals(updated.getOpen())));
        verify(authPathMapper).batchSave(argThat(paths -> paths.size() == 1
                && authApplyDTO.getPathList().get(0).equals(paths.get(0).getPath())));
        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        AppAuthData publishedData = (AppAuthData) eventCaptor.getValue().getSource().get(0);
        assertEquals(Boolean.TRUE, publishedData.getOpen());
    }

    private AuthApplyDTO buildAuthApplyDTO() {
        AuthApplyDTO authApplyDTO = new AuthApplyDTO();
        authApplyDTO.setAppKey("testAppKey");
        authApplyDTO.setAppName("testAppName");
        authApplyDTO.setAppParam("{\"appParam\":\"json\"}");
        authApplyDTO.setUserId("1");
        authApplyDTO.setExtInfo("{\"extInfo\":\"json\"}");
        authApplyDTO.setOpen(true);
        authApplyDTO.setPhone("18600000000");
        authApplyDTO.setPathList(Collections.singletonList("/testPath"));
        return authApplyDTO;
    }

    private AppAuthDTO buildAppAuthDTO() {
        return buildAppAuthDTO(null);
    }

    private AppAuthDTO buildAppAuthDTO(final String id) {
        AppAuthDTO appAuthDTO = new AppAuthDTO();
        appAuthDTO.setId(id);
        appAuthDTO.setAppKey("testAppKey");
        appAuthDTO.setAppSecret("testAppSecret");
        appAuthDTO.setUserId("1");
        appAuthDTO.setPhone("18600000000");
        appAuthDTO.setExtInfo("{\"extInfo\":\"json\"}");
        appAuthDTO.setOpen(true);
        appAuthDTO.setEnabled(false);
        return appAuthDTO;
    }

    private AppAuthDO buildAppAuthDO() {
        AppAuthDO appAuthDO = AppAuthDO.create(buildAppAuthDTO());
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        appAuthDO.setDateCreated(now);
        appAuthDO.setDateUpdated(now);
        return appAuthDO;
    }

    private AppAuthDO buildAppAuthDO(final String id, final String namespaceId) {
        AppAuthDO auth = buildAppAuthDO();
        auth.setId(id);
        auth.setAppKey(id + "-key");
        auth.setNamespaceId(namespaceId);
        return auth;
    }

    private AuthParamDTO buildAuthParamDTO() {
        AuthParamDTO authParamDTO = new AuthParamDTO();
        authParamDTO.setAppName("testAppName");
        authParamDTO.setAppParam("{\"type\":\"test\"}");
        return authParamDTO;
    }

    private AuthPathDTO buildAuthPathDTO() {
        AuthPathDTO authPathDTO = new AuthPathDTO();
        authPathDTO.setAppName("testAppName");
        authPathDTO.setPath("/test");
        return authPathDTO;
    }
}
