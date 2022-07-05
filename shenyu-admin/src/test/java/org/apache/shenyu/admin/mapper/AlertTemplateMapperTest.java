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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for AlertTemplateMapper.
 */
public final class AlertTemplateMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private AlertTemplateMapper alertTemplateMapper;

    private final AlertTemplateDTO alertTemplateDTO = buildAlertTemplateDTO();

    @BeforeEach
    public void before() {
        int count = alertTemplateMapper.insertSelective(alertTemplateDTO);
        assertEquals(1, count);
    }

    @Test
    public void testInsertSelective() {
        AlertTemplateDTO newAlertTemplateDTO = buildAlertTemplateDTO();
        int count = alertTemplateMapper.insertSelective(newAlertTemplateDTO);
        assertEquals(1, count);
    }

    @Test
    public void testSelectById() {
        AlertTemplateDO alertTemplateDO = alertTemplateMapper.selectByPrimaryKey(alertTemplateDTO.getId());
        assertNotNull(alertTemplateDO);
    }

    @Test
    public void testSelectAll() {
        List<AlertTemplateVO> alertTemplateVOS = alertTemplateMapper.selectAll();
        assertThat(alertTemplateVOS.size(), greaterThan(0));
    }

    @Test
    public void testUpdateSelective() {
        int count = alertTemplateMapper.updateByPrimaryKeySelective(alertTemplateDTO);
        assertEquals(1, count);
    }

    @Test
    public void deleteByIdSet() {
        List<AlertTemplateVO> alertTemplateVOS = alertTemplateMapper.selectAll();
        List<String> idList = alertTemplateVOS.stream().map(item -> String.valueOf(item.getId())).collect(Collectors.toList());
        int result = alertTemplateMapper.deleteByIds(idList);
        assertThat(result, comparesEqualTo(idList.size()));
    }

    private AlertTemplateDTO buildAlertTemplateDTO() {
        AlertTemplateDTO alertTemplateDTO = new AlertTemplateDTO();
        alertTemplateDTO.setId(1L);
        alertTemplateDTO.setName("testName");
        alertTemplateDTO.setStrategyName("testStrategyName");
        alertTemplateDTO.setContent("testContent");
        return alertTemplateDTO;
    }

}
