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
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.publish.MetaDataEventPublisher;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
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

    @BeforeAll
    public static void beforeClass() {
        loggerSpy = spy(LoggerFactory.getLogger(MetaDataServiceImpl.class));
        loggerFactoryMockedStatic = mockStatic(LoggerFactory.class);
        loggerFactoryMockedStatic.when(() -> LoggerFactory.getLogger(MetaDataServiceImpl.class)).thenReturn(loggerSpy);
    }

    @AfterAll
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
        String msg = metaDataService.enabled(ids, true);
        assertEquals(AdminConstants.ID_NOT_EXIST, msg);
        when(metaDataMapper.selectByIdList(ids))
                .thenReturn(Arrays.asList(MetaDataDO.builder().build(), MetaDataDO.builder().build()))
                .thenReturn(Arrays.asList(MetaDataDO.builder().build(), MetaDataDO.builder().build(), MetaDataDO.builder().build()));
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
        when(metaDataMapper.pathExisted(any())).thenReturn(null);
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
        when(metaDataMapper.pathExistedExclude("path", Collections.singletonList("id"))).thenReturn(null);
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
        int count = metaDataService.delete(ids);
        Assertions.assertEquals(0, count,
                "The count of delete should be 0.");
    }

    /**
     * Cases where get a not empty id list.
     */
    private void testDeleteForNotEmptyIds() {
        List<String> ids = Lists.newArrayList("id1", "id3");
        int count = metaDataService.delete(ids);
        Assertions.assertEquals(0, count, "The count of delete should be 0.");
        when(metaDataMapper.selectByIdList(ids)).thenReturn(Arrays.asList(MetaDataDO.builder().build(), MetaDataDO.builder().build()));
        when(metaDataMapper.deleteByIdList(ids)).thenReturn(2);
        count = metaDataService.delete(ids);
        Assertions.assertEquals(2, count,
                "The count of delete should be 2.");
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
