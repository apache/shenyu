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
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for MetaDataService.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(MetaDataServiceImpl.class)
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
    private MetaDataDTO metaDataDTO;

    @Mock
    private MetaDataQuery metaDataQuery;

    @BeforeClass
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(MetaDataServiceImpl.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(MetaDataServiceImpl.class)).thenReturn(loggerSpy);
    }

    @AfterClass
    public static void afterClass() {
        loggerFactoryMockedStatic.close();
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
     * Test case for createOrUpdate.<br>
     * Note that the following methods have dependencies before and after.
     */
    @Test
    public void testCreateOrUpdate() {
        doNothing().when(loggerSpy).error(anyString(), isA(MetaDataDTO.class));
        testCreateOrUpdateForParamsError();
        testCreateOrUpdateForPathExist();
        testCreateOrUpdateForInsert();
        testCreateOrUpdateForUpdate();
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
        when(metaDataMapper.selectById(anyString()))
                .thenReturn(MetaDataDO.builder().build())
                .thenReturn(null)
                .thenReturn(MetaDataDO.builder().build());
        String msg = metaDataService.enabled(ids, true);
        assertEquals(AdminConstants.ID_NOT_EXIST, msg);

        msg = metaDataService.enabled(ids, false);
        assertEquals(StringUtils.EMPTY, msg);
    }

    /**
     * Test case for syncData.
     */
    @Test
    public void testSyncDate() {
        ArrayList<MetaDataDO> all = Lists.newArrayList(MetaDataDO.builder().build());
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
        Assert.assertEquals(new MetaDataVO(), dataVo);

        final String appName = "appName";
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        metaDataDO.setAppName(appName);
        when(metaDataMapper.selectById(anyString())).thenReturn(metaDataDO);
        dataVo = metaDataService.findById(anyString());
        Assert.assertEquals(appName, dataVo.getAppName());
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
        Assert.assertEquals("The dataList should be contain " + metaDataDOList.size() + " element.",
                metaDataDOList.size(), pager.getDataList().size());
    }

    /**
     * Test case for findAll.
     */
    @Test
    public void testFindAll() {
        ArrayList<MetaDataDO> metaDataDOList = getMetaDataDOList();
        when(metaDataMapper.selectAll()).thenReturn(metaDataDOList);
        List<MetaDataVO> all = metaDataService.findAll();
        Assert.assertEquals("The list should be contain " + metaDataDOList.size() + " element.",
                metaDataDOList.size(), all.size());
    }

    /**
     * Test case for findAllGroup.
     */
    @Test
    public void testFindAllGroup() {
        when(metaDataMapper.selectAll()).thenReturn(getMetaDataDOList());
        Map<String, List<MetaDataVO>> allGroup = metaDataService.findAllGroup();
        Assert.assertEquals("There should be 3 groups.", 3, allGroup.keySet().size());
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
        Assert.assertEquals("The List should be contain " + (metaDataDOList.size() - 1) + " element.",
                metaDataDOList.size() - 1, all.size());
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
     * Cases where the params error.
     */
    private void testCreateOrUpdateForParamsError() {
        when(metaDataDTO.getAppName())
                .thenReturn(null)
                .thenReturn(StringUtils.EMPTY)
                .thenReturn("AppName");
        when(metaDataDTO.getPath())
                .thenReturn(null)
                .thenReturn(StringUtils.EMPTY)
                .thenReturn("path");
        when(metaDataDTO.getRpcType())
                .thenReturn(null)
                .thenReturn(StringUtils.EMPTY)
                .thenReturn("rpcType");
        when(metaDataDTO.getServiceName())
                .thenReturn(null)
                .thenReturn(StringUtils.EMPTY)
                .thenReturn("serviceName");
        when(metaDataDTO.getMethodName())
                .thenReturn(null)
                .thenReturn(StringUtils.EMPTY)
                .thenReturn("methodName");

        for (int i = 0; i < 2 * 5; i++) {
            String msg = metaDataService.createOrUpdate(metaDataDTO);
            assertEquals(AdminConstants.PARAMS_ERROR, msg);
        }
    }

    /**
     * Cases where check passed or the data path already exists.<br>
     * The stub declared in createOrUpdateCase1 will not be repeated.
     */
    private void testCreateOrUpdateForPathExist() {
        MetaDataDO metaDataDO = MetaDataDO.builder().id("id1").build();
        when(metaDataDTO.getId())
                .thenReturn(null)
                .thenReturn("id1");
        when(metaDataMapper.findByPath(anyString()))
                .thenReturn(null)
                .thenReturn(metaDataDO);

        for (int i = 0; i < 2; i++) {
            String msg = metaDataService.createOrUpdate(metaDataDTO);
            assertEquals(StringUtils.EMPTY, msg);
        }

        when(metaDataDTO.getId()).thenReturn("id2");
        String msg = metaDataService.createOrUpdate(metaDataDTO);
        assertEquals(AdminConstants.DATA_PATH_IS_EXIST, msg);
    }

    /**
     * Cases where check passed and insert operation.<br>
     * The stub declared in createOrUpdateCase1 will not be repeated.
     */
    private void testCreateOrUpdateForInsert() {
        when(metaDataDTO.getId()).thenReturn(null);
        when(metaDataMapper.findByPath(anyString())).thenReturn(null);
        when(metaDataMapper.insert(any())).thenReturn(1);
        String msg = metaDataService.createOrUpdate(metaDataDTO);
        assertEquals(StringUtils.EMPTY, msg);
    }

    /**
     * Cases where check passed and update operation.<br>
     * The stub declared in createOrUpdateCase1 and createOrUpdateCase3 will not be repeated.
     */
    private void testCreateOrUpdateForUpdate() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataDTO.getId()).thenReturn("id");
        when(metaDataMapper.selectById("id")).thenReturn(null).thenReturn(metaDataDO);
        when(metaDataMapper.update(any())).thenReturn(1);
        String msg = metaDataService.createOrUpdate(metaDataDTO);
        assertEquals(StringUtils.EMPTY, msg);
    }

    private void assertEquals(final String expected, final String actual) {
        Assert.assertEquals("The msg should be '" + expected + "'.",
                expected, actual);
    }

    /**
     * Cases where get an empty id list.
     */
    private void testDeleteForEmptyIds() {
        List<String> ids = Lists.newArrayList();
        int count = metaDataService.delete(ids);
        Assert.assertEquals("The count of delete should be 0.",
                0, count);
    }

    /**
     * Cases where get a not empty id list.
     */
    private void testDeleteForNotEmptyIds() {
        List<String> ids = Lists.newArrayList("id1", "id2", "id3");
        when(metaDataMapper.selectById("id1")).thenReturn(MetaDataDO.builder().build());
        when(metaDataMapper.selectById("id3")).thenReturn(MetaDataDO.builder().build());
        when(metaDataMapper.delete("id1")).thenReturn(1);
        when(metaDataMapper.delete("id3")).thenReturn(1);
        int count = metaDataService.delete(ids);
        Assert.assertEquals("The count of delete should be 2.",
                2, count);
    }

    private ArrayList<MetaDataDO> getMetaDataDOList() {
        final MetaDataDO metaDataDO1 = MetaDataDO.builder()
                .id("id1")
                .appName("appName1")
                .build();
        final MetaDataDO metaDataDO2 = MetaDataDO.builder()
                .id("id2")
                .appName("appName2")
                .build();
        final MetaDataDO metaDataDO3 = MetaDataDO.builder()
                .id("id3")
                .appName("appName3")
                .build();
        return Lists.newArrayList(metaDataDO1, metaDataDO2, metaDataDO3);
    }
}
