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
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.service.impl.TagRelationServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for TagRelationService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class TagRelationServiceTest {

    @InjectMocks
    private TagRelationServiceImpl tagRelationService;

    @Mock
    private TagRelationMapper tagRelationMapper;

    @Test
    public void testCreate() {
        TagRelationDTO tagRelationDTO = buildTagRelationDTO();
        given(this.tagRelationMapper.insert(any())).willReturn(1);
        int cnt = tagRelationService.create(tagRelationDTO);
        assertEquals(cnt, 1);
    }

    @Test
    public void testUpdate() {
        TagRelationDTO tagRelationDTO = buildTagRelationDTO();
        given(this.tagRelationMapper.updateByPrimaryKeySelective(any())).willReturn(1);
        given(this.tagRelationMapper.selectByPrimaryKey(any())).willReturn(buildTagRelationDO());
        int cnt = tagRelationService.update(tagRelationDTO);
        assertEquals(cnt, 1);
    }

    @Test
    public void testDelete() {
        given(this.tagRelationMapper.deleteByIds(any())).willReturn(1);
        int cnt = tagRelationService.delete(Lists.newArrayList("11111"));
        assertEquals(cnt, 1);
    }

    @Test
    public void testFindId() {
        given(this.tagRelationMapper.selectByPrimaryKey(any())).willReturn(buildTagRelationDO());
        TagRelationDO tagRelationDO = tagRelationService.findById("11111");
        assertNotNull(tagRelationDO);
    }

    @Test
    public void testFindByTagId() {
        List<TagRelationDO> tagRelationDOList = new ArrayList<>();
        tagRelationDOList.add(buildTagRelationDO());
        given(this.tagRelationMapper.selectByQuery(any())).willReturn(tagRelationDOList);
        List<TagRelationDO> tagRelationDOS = tagRelationService.findByTagId("123");
        assertEquals(tagRelationDOS.size(), 1);
    }

    @Test
    public void testFindByApiId() {
        List<TagRelationDO> tagRelationDOList = new ArrayList<>();
        tagRelationDOList.add(buildTagRelationDO());
        given(this.tagRelationMapper.selectByQuery(any())).willReturn(tagRelationDOList);
        List<TagRelationDO> tagRelationDOS = tagRelationService.findApiId("123456");
        assertEquals(tagRelationDOS.size(), 1);
    }

    private TagRelationDO buildTagRelationDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagRelationDO tagRelationDO = TagRelationDO.builder()
                .tagId("123")
                .id("11111")
                .apiId("123456")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagRelationDO;
    }

    private TagRelationDTO buildTagRelationDTO() {
        TagRelationDTO tagRelationDTO = new TagRelationDTO();
        tagRelationDTO.setTagId("123");
        tagRelationDTO.setId("11111");
        tagRelationDTO.setApiId("123456");
        return tagRelationDTO;
    }

}
