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

import org.apache.shenyu.admin.mapper.DetailMapper;
import org.apache.shenyu.admin.model.dto.DetailDTO;
import org.apache.shenyu.admin.model.entity.DetailDO;
import org.apache.shenyu.admin.model.vo.DetailVO;
import org.apache.shenyu.admin.service.impl.DetailServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for DetailService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DetailServiceTest {

    private final Integer resultInt = 0;

    @InjectMocks
    private DetailServiceImpl detailService;

    @Mock
    private DetailMapper detailMapper;

    @BeforeEach
    public void setUp() {
        detailService = new DetailServiceImpl(detailMapper);
    }

    @Test
    public void testCreateOrUpdate() {
        testCreate();
        testUpdate("123");
    }

    @Test
    public void testDelete() {
        testCreate();
        when(detailMapper.deleteByPrimaryKey("123")).thenReturn(1);
        assertEquals(1, detailService.delete("123"));
    }

    @Test
    public void testFindById() {
        DetailDO detailDO = new DetailDO();
        detailDO.setId("1");
        detailDO.setExample(Boolean.TRUE);
        detailDO.setValueDesc("value desc");
        detailDO.setFieldValue("field value");
        detailDO.setFieldId("field id");

        when(detailMapper.selectByPrimaryKey("1")).thenReturn(detailDO);

        DetailService detailService = new DetailServiceImpl(detailMapper);

        DetailVO detailVO = detailService.findById("1");

        assertEquals(detailDO.getId(), detailVO.getId());
        assertEquals(detailDO.getExample(), detailVO.getExample());
        assertEquals(detailDO.getValueDesc(), detailVO.getValueDesc());
        assertEquals(detailDO.getFieldValue(), detailVO.getFieldValue());
        assertEquals(detailDO.getFieldId(), detailVO.getFieldId());
        assertEquals(detailDO.getDateUpdated(), detailVO.getDateUpdated());
        assertEquals(detailDO.getDateCreated(), detailVO.getDateCreated());

        verify(detailMapper, times(1)).selectByPrimaryKey("1");
    }

    private void testCreate() {
        DetailDTO detailDTO = buildDetailDTO("");
        assertEquals(resultInt, this.detailService.createOrUpdate(detailDTO));
    }

    private void testUpdate(final String id) {
        DetailDTO detailDTO = new DetailDTO();
        detailDTO.setId(id);
        detailDTO.setExample(Boolean.FALSE);
        detailDTO.setFieldId("testDocument");
        assertEquals(resultInt, this.detailService.createOrUpdate(detailDTO));
    }

    private DetailDTO buildDetailDTO(final String id) {
        DetailDTO detailDTO = new DetailDTO();
        if (StringUtils.isNotBlank(id)) {
            detailDTO.setId(id);
        }
        detailDTO.setDateCreated(new Timestamp(new Date().getTime()));
        detailDTO.setExample(Boolean.TRUE);
        detailDTO.setDateCreated(new Timestamp(new Date().getTime()));
        detailDTO.setFieldId("string");
        detailDTO.setFieldValue("string");
        detailDTO.setValueDesc("string");
        return detailDTO;
    }
}
