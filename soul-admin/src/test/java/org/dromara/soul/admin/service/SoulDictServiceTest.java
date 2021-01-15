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

package org.dromara.soul.admin.service;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.BatchCommonDTO;
import org.dromara.soul.admin.dto.SoulDictDTO;
import org.dromara.soul.admin.entity.SoulDictDO;
import org.dromara.soul.admin.mapper.SoulDictMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.SoulDictQuery;
import org.dromara.soul.admin.service.impl.SoulDictServiceImpl;
import org.dromara.soul.admin.vo.SoulDictVO;
import org.dromara.soul.common.utils.UUIDUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for SoulDictService.
 *
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulDictServiceTest {

    @InjectMocks
    private SoulDictServiceImpl soulDictService;

    @Mock
    private SoulDictMapper soulDictMapper;

    @Test
    public void testFindByType() {
        SoulDictDO soulDictDO = buildSoulDictDO();
        given(this.soulDictMapper.selectByQuery(any())).willReturn(Collections.singletonList(soulDictDO));
        List<SoulDictVO> soulDictVOList = this.soulDictService.list("rule");
        assertEquals(1, soulDictVOList.size());
        assertEquals(soulDictDO.getId(), soulDictVOList.get(0).getId());
    }

    @Test
    public void testFindById() {
        SoulDictDO soulDictDO = buildSoulDictDO();
        given(this.soulDictMapper.selectById(eq("123"))).willReturn(soulDictDO);
        SoulDictVO soulDictVO = this.soulDictService.findById("123");
        assertNotNull(soulDictVO);
        assertEquals(soulDictDO.getId(), soulDictVO.getId());
    }

    @Test
    public void testCreateOrUpdate() {
        SoulDictDTO insertSoulDictDTO = buildSoulDictDTO();
        given(this.soulDictMapper.insertSelective(any())).willReturn(1);
        assertThat(this.soulDictService.createOrUpdate(insertSoulDictDTO), greaterThan(0));
        SoulDictDTO updateSoulDictDTO = buildSoulDictDTO(UUIDUtils.getInstance().generateShortUuid());
        given(this.soulDictMapper.updateByPrimaryKeySelective(any())).willReturn(1);
        assertThat(this.soulDictService.createOrUpdate(updateSoulDictDTO), greaterThan(0));
    }

    @Test
    public void testDeleteSoulDicts() {
        given(this.soulDictMapper.delete(eq("123"))).willReturn(1);
        int count = soulDictService.deleteSoulDicts(Collections.singletonList("123"));
        assertThat(count, greaterThan(0));
    }

    @Test
    public void testBatchEnabled() {
        BatchCommonDTO batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(false);
        Integer idNullResult = this.soulDictService.enabled(batchCommonDTO.getIds(), false);
        assertThat(idNullResult, comparesEqualTo(0));
        batchCommonDTO.setIds(new ArrayList<>());
        Integer idEmptyResult = this.soulDictService.enabled(batchCommonDTO.getIds(), false);
        assertThat(idEmptyResult, comparesEqualTo(0));
        batchCommonDTO.setIds(Collections.singletonList("123"));
        given(this.soulDictMapper.enabled(eq(batchCommonDTO.getIds()), eq(batchCommonDTO.getEnabled()))).willReturn(1);
        assertThat(this.soulDictService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled()), greaterThan(0));
    }

    @Test
    public void testListByPage() {
        PageParameter pageParameter = new PageParameter();
        pageParameter.setPageSize(5);
        pageParameter.setTotalCount(10);
        pageParameter.setTotalPage(pageParameter.getTotalCount() / pageParameter.getPageSize());
        SoulDictQuery soulDictQuery = new SoulDictQuery("1", "t", "t_n", pageParameter);
        given(this.soulDictMapper.countByQuery(soulDictQuery)).willReturn(10);
        List<SoulDictDO> soulDictDOList = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            SoulDictDO soulDictVO = buildSoulDictDO();
            soulDictDOList.add(soulDictVO);
        }
        given(this.soulDictMapper.selectByQuery(soulDictQuery)).willReturn(soulDictDOList);
        final CommonPager<SoulDictVO> pluginDOCommonPager = this.soulDictService.listByPage(soulDictQuery);
        assertEquals(pluginDOCommonPager.getDataList().size(), soulDictDOList.size());
    }

    private SoulDictDTO buildSoulDictDTO() {
        return buildSoulDictDTO("");
    }

    private SoulDictDTO buildSoulDictDTO(final String id) {
        SoulDictDTO soulDictDTO = new SoulDictDTO();
        if (StringUtils.isNotBlank(id)) {
            soulDictDTO.setId(id);
        }
        soulDictDTO.setDesc("test");
        soulDictDTO.setSort(1);
        soulDictDTO.setDesc("test");
        soulDictDTO.setDictCode("t_dict_1");
        soulDictDTO.setDictName("t_d_v");
        soulDictDTO.setEnabled(false);
        soulDictDTO.setType("rule");
        return soulDictDTO;
    }

    private SoulDictDO buildSoulDictDO() {
        SoulDictDO soulDictDO = SoulDictDO.buildSoulDictDO(buildSoulDictDTO());
        Optional.ofNullable(soulDictDO).ifPresent(it -> {
            Timestamp now = Timestamp.valueOf(LocalDateTime.now());
            it.setDateCreated(now);
            it.setDateUpdated(now);
        });
        return soulDictDO;
    }
}
