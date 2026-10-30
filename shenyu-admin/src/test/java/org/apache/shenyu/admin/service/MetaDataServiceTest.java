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
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.publish.MetaDataEventPublisher;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shiro.authz.UnauthorizedException;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for MetaDataService.
 */
@ExtendWith(MockitoExtension.class)
public final class MetaDataServiceTest {

    private static Logger loggerSpy;

    private static MockedStatic<LoggerFactory> loggerFactoryMockedStatic;

    @InjectMocks
    private MetaDataServiceImpl metaDataService;

    @Mock
    private MetaDataMapper metaDataMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private MetaDataEventPublisher publisher;

    @Mock
    private MetaDataDTO metaDataDTO;

    @Mock
    private MetaDataQuery metaDataQuery;

    @Mock
    private NamespaceMapper namespaceMapper;

    @Mock
    private Subject subject;

    @BeforeAll
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(MetaDataServiceImpl.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class, CALLS_REAL_METHODS);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(MetaDataServiceImpl.class)).thenReturn(loggerSpy);
    }

    @AfterAll
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
    }

    @BeforeEach
    public void setUp() {
        ThreadContext.bind(subject);
    }

    @AfterEach
    public void tearDown() {
        ThreadContext.unbindSubject();
    }

    /**
     * Test case for saveOrUpdateMetaData().
     */
    @Test
    public void testSaveOrUpdateMetaData() {
        testSaveOrUpdateMetaDataForInsert();
        testSaveOrUpdateMetaDataForUpdate();
    }

    /**
     * Test case for re-registering disabled metadata keeps it disabled on the gateway.
     */
    @Test
    public void testSaveOrUpdateMetaDataKeepsExistingEnabledStatus() {
        MetaDataDO exist = MetaDataDO.builder().id("1").enabled(false).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        MetaDataRegisterDTO registerDTO = new MetaDataRegisterDTO();
        registerDTO.setPath("/svc/m1");
        registerDTO.setEnabled(true);

        metaDataService.saveOrUpdateMetaData(exist, registerDTO);

        ArgumentCaptor<MetaDataDO> doCaptor = ArgumentCaptor.forClass(MetaDataDO.class);
        verify(metaDataMapper).update(doCaptor.capture());
        Assertions.assertFalse(doCaptor.getValue().getEnabled());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        List<?> source = (List<?>) captor.getValue().getSource();
        Assertions.assertEquals(1, source.size());
        Assertions.assertFalse(((MetaData) source.get(0)).getEnabled());
    }

    /**
     * Test case for createOrUpdate.<br>
     * Note that the following methods have dependencies before and after.
     */
    @Test
    public void testCreateOrUpdate() {
        testCreateOrUpdateForInsert();
        testCreateOrUpdateForUpdate();
    }

    /**
     * Test case for an add-only subject creating metadata with a blank id.
     */
    @Test
    public void testAddOnlyPermissionAllowsCreateWithBlankId() {
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId(" ");
        dto.setPath("path");
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        when(metaDataMapper.pathExisted("path", SYS_DEFAULT_NAMESPACE_ID)).thenReturn(null);
        when(metaDataMapper.insert(any())).thenReturn(1);

        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, metaDataService.createOrUpdate(dto));

        verify(subject).checkPermission("system:meta:add");
        verify(subject, never()).checkPermission("system:meta:edit");
        verify(metaDataMapper).insert(any());
    }

    /**
     * Test case for an edit-only subject updating existing metadata.
     */
    @Test
    public void testEditOnlyPermissionAllowsUpdate() {
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId("id");
        dto.setPath("path");
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        MetaDataDO current = MetaDataDO.builder().id("id").enabled(true).build();
        when(metaDataMapper.pathExistedExclude("path", SYS_DEFAULT_NAMESPACE_ID, Collections.singletonList("id"))).thenReturn(null);
        when(metaDataMapper.selectById("id")).thenReturn(current);
        when(metaDataMapper.update(any())).thenReturn(1);

        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, metaDataService.createOrUpdate(dto));

        verify(subject).checkPermission("system:meta:edit");
        verify(subject, never()).checkPermission("system:meta:add");
        verify(metaDataMapper).update(any());
    }

    /**
     * Test case for an edit-only subject attempting to create metadata.
     */
    @Test
    public void testEditOnlyPermissionCannotCreate() {
        MetaDataDTO dto = new MetaDataDTO();
        doThrow(new UnauthorizedException()).when(subject).checkPermission("system:meta:add");

        assertThrows(UnauthorizedException.class, () -> metaDataService.createOrUpdate(dto));

        verifyNoInteractions(metaDataMapper, eventPublisher, publisher);
    }

    /**
     * Test case for an add-only subject attempting to update a missing metadata id.
     */
    @Test
    public void testAddOnlyPermissionCannotUpdateMissingMetadata() {
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId("missing");
        doThrow(new UnauthorizedException()).when(subject).checkPermission("system:meta:edit");

        assertThrows(UnauthorizedException.class, () -> metaDataService.createOrUpdate(dto));

        verifyNoInteractions(metaDataMapper, eventPublisher, publisher);
    }

    /**
     * Test case for a subject without either metadata write permission.
     */
    @Test
    public void testNoPermissionRejectsCreateAndUpdate() {
        doThrow(new UnauthorizedException()).when(subject).checkPermission(anyString());
        MetaDataDTO create = new MetaDataDTO();
        MetaDataDTO update = new MetaDataDTO();
        update.setId("id");

        assertThrows(UnauthorizedException.class, () -> metaDataService.createOrUpdate(create));
        assertThrows(UnauthorizedException.class, () -> metaDataService.createOrUpdate(update));

        verifyNoInteractions(metaDataMapper, eventPublisher, publisher);
    }

    /**
     * Test case for update publishes sibling methods whose rpc_ext changed.
     */
    @Test
    public void testUpdatePublishesSiblingRpcExtChanges() {
        final String newRpcExt = "{\"timeout\":3000}";
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId("id1");
        dto.setPath("/svc/m1");
        dto.setServiceName("svc");
        dto.setMethodName("m1");
        dto.setRpcExt(newRpcExt);
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        MetaDataDO current = MetaDataDO.builder().id("id1").path("/svc/m1").serviceName("svc").methodName("m1")
                .rpcExt("{}").enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        MetaDataDO staleSibling = MetaDataDO.builder().id("id2").path("/svc/m2").serviceName("svc").methodName("m2")
                .rpcExt("{}").enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        MetaDataDO syncedSibling = MetaDataDO.builder().id("id3").path("/svc/m3").serviceName("svc").methodName("m3")
                .rpcExt(newRpcExt).enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        when(metaDataMapper.pathExistedExclude("/svc/m1", SYS_DEFAULT_NAMESPACE_ID, Collections.singletonList("id1"))).thenReturn(null);
        when(metaDataMapper.selectById("id1")).thenReturn(current);
        when(metaDataMapper.update(any())).thenReturn(1);
        when(metaDataMapper.findByServiceNameAndMethodAndNamespaceId("svc", null, SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(Arrays.asList(current, staleSibling, syncedSibling));

        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, metaDataService.createOrUpdate(dto));

        verify(metaDataMapper, times(2)).update(any());
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        List<?> source = (List<?>) captor.getValue().getSource();
        List<String> paths = source.stream().map(o -> ((MetaData) o).getPath()).collect(Collectors.toList());
        Assertions.assertEquals(Arrays.asList("/svc/m1", "/svc/m2"), paths);
        source.forEach(o -> Assertions.assertEquals(newRpcExt, ((MetaData) o).getRpcExt()));
    }

    /**
     * Test case for no events when the current metadata update returns zero.
     */
    @Test
    public void testUpdateDoesNotPublishWhenCurrentUpdateReturnsZero() {
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId("id1");
        dto.setPath("/svc/m1");
        dto.setServiceName("svc");
        dto.setMethodName("m1");
        dto.setRpcExt("{\"timeout\":3000}");
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        MetaDataDO current = MetaDataDO.builder().id("id1").path("/svc/m1").serviceName("svc").methodName("m1")
                .rpcExt("{}").enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        when(metaDataMapper.pathExistedExclude("/svc/m1", SYS_DEFAULT_NAMESPACE_ID, Collections.singletonList("id1"))).thenReturn(null);
        when(metaDataMapper.selectById("id1")).thenReturn(current);
        when(metaDataMapper.update(any())).thenReturn(0);

        metaDataService.createOrUpdate(dto);

        verify(metaDataMapper).update(any());
        verify(metaDataMapper, never()).findByServiceNameAndMethodAndNamespaceId(any(), any(), any());
        verifyNoInteractions(eventPublisher, publisher);
    }

    /**
     * Test case for excluding a sibling whose metadata update returns zero from the event.
     */
    @Test
    public void testUpdateDoesNotPublishSiblingWhenUpdateReturnsZero() {
        final String newRpcExt = "{\"timeout\":3000}";
        MetaDataDTO dto = new MetaDataDTO();
        dto.setId("id1");
        dto.setPath("/svc/m1");
        dto.setServiceName("svc");
        dto.setMethodName("m1");
        dto.setRpcExt(newRpcExt);
        dto.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        MetaDataDO current = MetaDataDO.builder().id("id1").path("/svc/m1").serviceName("svc").methodName("m1")
                .rpcExt("{}").enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        MetaDataDO sibling = MetaDataDO.builder().id("id2").path("/svc/m2").serviceName("svc").methodName("m2")
                .rpcExt("{}").enabled(true).namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        when(metaDataMapper.pathExistedExclude("/svc/m1", SYS_DEFAULT_NAMESPACE_ID, Collections.singletonList("id1"))).thenReturn(null);
        when(metaDataMapper.selectById("id1")).thenReturn(current);
        when(metaDataMapper.update(any())).thenReturn(1, 0);
        when(metaDataMapper.findByServiceNameAndMethodAndNamespaceId("svc", null, SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(Arrays.asList(current, sibling));

        metaDataService.createOrUpdate(dto);

        verify(metaDataMapper, times(2)).update(any());
        verify(publisher).onUpdated(any(MetaDataDO.class), any(MetaDataDO.class));
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        List<?> source = captor.getValue().getSource();
        Assertions.assertEquals(1, source.size());
        MetaData changed = (MetaData) source.get(0);
        Assertions.assertEquals("id1", changed.getId());
        Assertions.assertEquals("/svc/m1", changed.getPath());
        Assertions.assertEquals(newRpcExt, changed.getRpcExt());
    }

    /**
     * Test case for delete.<br>
     * Note that there is no test case where ids is null,
     * because the source code needs to be updated first.
     */
    @Test
    public void testDelete() {
        testDeleteForEmptyIds();
        testDeleteForNotEmptyIds();
    }

    /**
     * Test case for enabled.
     */
    @Test
    public void testEnabled() {
        List<String> ids = Lists.newArrayList("id1", "id2", "id3");
        String msg = metaDataService.enabledByIdsAndNamespaceId(ids, true, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(AdminConstants.ID_NOT_EXIST, msg);
        when(metaDataMapper.selectByIdListAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(Arrays.asList(MetaDataDO.builder().id("id1").build(), MetaDataDO.builder().id("id2").build()))
                .thenReturn(Arrays.asList(MetaDataDO.builder().id("id1").build(), MetaDataDO.builder().id("id2").build(), MetaDataDO.builder().id("id3").build()));
        msg = metaDataService.enabledByIdsAndNamespaceId(ids, false, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(StringUtils.EMPTY, msg);
        verify(publisher, never()).onEnabled(any());
        when(metaDataMapper.updateEnableBatch(ids, true)).thenReturn(ids.size());
        when(metaDataMapper.selectByIdListAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(Arrays.asList(MetaDataDO.builder().id("id1").build(), MetaDataDO.builder().id("id2").build(), MetaDataDO.builder().id("id3").build()));
        msg = metaDataService.enabledByIdsAndNamespaceId(ids, true, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(StringUtils.EMPTY, msg);
        verify(publisher).onEnabled(any());
    }

    /**
     * Test case for enabled only updates metadata that belongs to the current namespace.
     */
    @Test
    public void testEnabledOnlyUpdatesIdsWithinNamespace() {
        List<String> ids = Lists.newArrayList("id-in-current-namespace", "id-in-other-namespace");
        MetaDataDO inNamespace = MetaDataDO.builder().id("id-in-current-namespace").namespaceId(SYS_DEFAULT_NAMESPACE_ID).build();
        when(metaDataMapper.selectByIdListAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(Collections.singletonList(inNamespace));
        when(metaDataMapper.updateEnableBatch(Collections.singletonList("id-in-current-namespace"), true)).thenReturn(1);
        String msg = metaDataService.enabledByIdsAndNamespaceId(ids, true, SYS_DEFAULT_NAMESPACE_ID);
        assertEquals(StringUtils.EMPTY, msg);
        verify(metaDataMapper).updateEnableBatch(Collections.singletonList("id-in-current-namespace"), true);
        verify(publisher).onEnabled(Collections.singletonList(inNamespace));
    }

    /**
     * Test case for syncData.
     */
    @Test
    public void testSyncDate() {
        ArrayList<MetaDataDO> all = Lists.newArrayList(MetaDataDO.builder().namespaceId("test").build());
        when(metaDataMapper.findAll())
                .thenReturn(null)
                .thenReturn(Lists.newArrayList())
                .thenReturn(all);
        doNothing().when(eventPublisher).publishEvent(any());
        for (int i = 0; i < 3; i++) {
            metaDataService.syncData();
        }
        verify(eventPublisher, times(1)).publishEvent(any());
    }

    /**
     * Test case for findById.
     */
    @Test
    public void testFindById() {
        when(metaDataMapper.selectById(anyString())).thenReturn(null);
        MetaDataVO dataVo = metaDataService.findById(anyString());
        Assertions.assertEquals(new MetaDataVO(), dataVo);

        final String appName = "appName";
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        metaDataDO.setAppName(appName);
        when(metaDataMapper.selectById(anyString())).thenReturn(metaDataDO);
        dataVo = metaDataService.findById(anyString());
        assertEquals(appName, dataVo.getAppName());
    }

    /**
     * Test case for listByPage.
     */
    @Test
    public void testListByPage() {
        when(metaDataQuery.getPageParameter()).thenReturn(new PageParameter(1, 10, 5));
        ArrayList<MetaDataDO> metaDataDOList = getMetaDataDOList();
        when(metaDataMapper.selectByQuery(any())).thenReturn(metaDataDOList);
        CommonPager<MetaDataVO> pager = metaDataService.listByPage(metaDataQuery);
        Assertions.assertEquals(metaDataDOList.size(), pager.getDataList().size(),
                "The dataList should be contain " + metaDataDOList.size() + " element.");
    }

    /**
     * Test case for findAll.
     */
    @Test
    public void testFindAll() {
        ArrayList<MetaDataDO> metaDataDOList = getMetaDataDOList();
        when(metaDataMapper.selectAll()).thenReturn(metaDataDOList);
        List<MetaDataVO> all = metaDataService.findAll();
        Assertions.assertEquals(metaDataDOList.size(), all.size(),
                "The list should be contain " + metaDataDOList.size() + " element.");
    }

    /**
     * Test case for findAllGroup.
     */
    @Test
    public void testFindAllGroup() {
        when(metaDataMapper.selectAll()).thenReturn(getMetaDataDOList());
        Map<String, List<MetaDataVO>> allGroup = metaDataService.findAllGroup();
        Assertions.assertEquals(3, allGroup.keySet().size(),
                "There should be 3 groups.");
    }

    /**
     * Test case for listAll.
     */
    @Test
    public void testListAll() {
        ArrayList<MetaDataDO> metaDataDOList = getMetaDataDOList();
        metaDataDOList.add(null);
        when(metaDataMapper.selectAll()).thenReturn(metaDataDOList);
        List<MetaData> all = metaDataService.listAll();
        Assertions.assertEquals(metaDataDOList.size() - 1, all.size(),
                "The List should be contain " + (metaDataDOList.size() - 1) + " element.");
    }

    /**
     * Test case for listAllData.
     */
    @Test
    public void testListAllData() {
        ArrayList<MetaDataDO> metaDataDOList = getMetaDataDOList();
        metaDataDOList.add(null);
        when(metaDataMapper.selectAll()).thenReturn(metaDataDOList);
        List<MetaDataVO> all = metaDataService.listAllData();
        Assertions.assertEquals(metaDataDOList.size() - 1, all.size(),
                "The List should be contain " + (metaDataDOList.size() - 1) + " element.");
    }

    @Test
    public void testImportData() {
        List<MetaDataDO> metaDataDOList = getMetaDataDOList();
        when(metaDataMapper.selectAll()).thenReturn(metaDataDOList);

        final List<MetaDataDTO> metaDataDTOList = getMetaDataDTOList();
        given(this.metaDataMapper.insert(any())).willReturn(1);
        given(this.metaDataMapper.pathExisted(any(), any())).willReturn(null);

        ConfigImportResult configImportResult = this.metaDataService.importData(metaDataDTOList);

        assertNotNull(configImportResult);
        Assertions.assertEquals(configImportResult.getSuccessCount(), metaDataDTOList.size());

    }

    @Test
    public void testFindByPath() {
        final MetaDataDO metaDataDO1 = MetaDataDO.builder()
                .id("id1")
                .appName("appName1")
                .path("path1")
                .build();
        given(this.metaDataMapper.findByPathAndNamespaceId(any(), any())).willReturn(metaDataDO1);
        MetaDataDO metaDataDO = metaDataService.findByPathAndNamespaceId("path1", SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(metaDataDO);
        Assertions.assertEquals(metaDataDO, metaDataDO1);
    }

    @Test
    public void testFindByServiceNameAndMethodName() {
        final MetaDataDO metaDataDO1 = MetaDataDO.builder()
                .id("id1")
                .appName("appName1")
                .path("path1")
                .serviceName("serviceName1")
                .methodName("method1")
                .build();
        given(this.metaDataMapper.findByServiceNameAndMethodAndNamespaceId(any(), any(), any()))
                .willReturn(Collections.singletonList(metaDataDO1));
        MetaDataDO metaDataDO = metaDataService
                .findByServiceNameAndMethodNameAndNamespaceId("serviceName1", "method1", SYS_DEFAULT_NAMESPACE_ID);
        assertNotNull(metaDataDO);
        Assertions.assertEquals(metaDataDO, metaDataDO1);
    }

    @Test
    public void testInsert() {
        final MetaDataDO metaDataDO1 = MetaDataDO.builder()
                .id("id1")
                .appName("appName1")
                .path("path1")
                .build();
        given(this.metaDataMapper.insert(any())).willReturn(1);
        int inserted = metaDataService.insert(metaDataDO1);
        Assertions.assertEquals(inserted, 1);
    }

    private void testSaveOrUpdateMetaDataForInsert() {
        metaDataService.saveOrUpdateMetaData(null, new MetaDataRegisterDTO());
        verify(metaDataMapper).insert(any(MetaDataDO.class));
    }

    private void testSaveOrUpdateMetaDataForUpdate() {
        metaDataService.saveOrUpdateMetaData(MetaDataDO.builder().id("1").build(), new MetaDataRegisterDTO());
        verify(metaDataMapper).update(any(MetaDataDO.class));
    }

    /**
     * Cases where check passed and insert operation.<br>
     * The stub declared in createOrUpdateCase1 will not be repeated.
     */
    private void testCreateOrUpdateForInsert() {
        when(metaDataDTO.getId()).thenReturn(null);
        when(metaDataMapper.insert(any())).thenReturn(1);
        when(metaDataMapper.pathExisted(any(), any())).thenReturn(null);
        String msg = metaDataService.createOrUpdate(metaDataDTO);
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, msg);
    }

    /**
     * Cases where check passed and update operation.<br>
     * The stub declared in createOrUpdateCase1 and createOrUpdateCase3 will not be repeated.
     */
    private void testCreateOrUpdateForUpdate() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataDTO.getId()).thenReturn("id");
        when(metaDataDTO.getPath()).thenReturn("path");
        when(metaDataDTO.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(metaDataMapper.pathExistedExclude("path", SYS_DEFAULT_NAMESPACE_ID, Collections.singletonList("id"))).thenReturn(null);
        when(metaDataMapper.selectById("id")).thenReturn(metaDataDO);
        when(metaDataMapper.update(any())).thenReturn(1);

        String msg = metaDataService.createOrUpdate(metaDataDTO);
        assertEquals(ShenyuResultMessage.UPDATE_SUCCESS, msg);
    }

    private void assertEquals(final String expected, final String actual) {
        Assertions.assertEquals(expected, actual,
                "The msg should be '" + expected + "'.");
    }

    /**
     * Cases where get an empty id list.
     */
    private void testDeleteForEmptyIds() {
        List<String> ids = Lists.newArrayList();
        int count = metaDataService.deleteByIdsAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID);
        Assertions.assertEquals(0, count,
                "The count of delete should be 0.");
    }

    /**
     * Cases where get a not empty id list.
     */
    private void testDeleteForNotEmptyIds() {
        List<String> ids = Lists.newArrayList("id1", "id3");
        int count = metaDataService.deleteByIdsAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID);
        Assertions.assertEquals(0, count, "The count of delete should be 0.");
        when(metaDataMapper.selectByIdListAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID)).thenReturn(Arrays.asList(MetaDataDO.builder().build(), MetaDataDO.builder().build()));
        when(metaDataMapper.deleteByIdListAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID)).thenReturn(2);
        count = metaDataService.deleteByIdsAndNamespaceId(ids, SYS_DEFAULT_NAMESPACE_ID);
        Assertions.assertEquals(2, count,
                "The count of delete should be 2.");
    }

    private ArrayList<MetaDataDO> getMetaDataDOList() {
        final MetaDataDO metaDataDO1 = MetaDataDO.builder()
                .id("id1")
                .appName("appName1")
                .path("path1")
                .build();
        final MetaDataDO metaDataDO2 = MetaDataDO.builder()
                .id("id2")
                .appName("appName2")
                .path("path2")
                .build();
        final MetaDataDO metaDataDO3 = MetaDataDO.builder()
                .id("id3")
                .appName("appName3")
                .path("path3")
                .build();
        return Lists.newArrayList(metaDataDO1, metaDataDO2, metaDataDO3);
    }

    private ArrayList<MetaDataDTO> getMetaDataDTOList() {
        final MetaDataDTO metaDataDTO1 = new MetaDataDTO();
        metaDataDTO1.setId("id123");
        metaDataDTO1.setAppName("appName123");
        metaDataDTO1.setPath("appName123");
        final MetaDataDTO metaDataDTO2 = new MetaDataDTO();
        metaDataDTO1.setId("id234");
        metaDataDTO1.setAppName("appName234");
        metaDataDTO1.setPath("appName234");
        final MetaDataDTO metaDataDTO3 = new MetaDataDTO();
        metaDataDTO1.setId("id345");
        metaDataDTO1.setAppName("appName345");
        metaDataDTO1.setPath("appName345");
        return Lists.newArrayList(metaDataDTO1, metaDataDTO2, metaDataDTO3);
    }
}
