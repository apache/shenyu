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
import java.util.List;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.impl.TagServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for TagService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class TagServiceTest {

    @Mock
    private TagMapper tagMapper;

    @InjectMocks
    private TagServiceImpl tagService;

    @Test
    public void testCreate() {
        given(this.tagMapper.insert(any())).willReturn(1);
        int cnt = tagService.create(buildTagDTO());
        assertEquals(cnt, 1);
    }

    @Test
    public void testUpdate() {
        TagDTO tagDTO = buildTagDTO();
        given(this.tagMapper.updateByPrimaryKeySelective(any())).willReturn(1);
        given(this.tagMapper.selectByPrimaryKey(any())).willReturn(buildTagDO());
        int cnt = tagService.update(tagDTO);
        assertEquals(cnt, 1);
    }

    @Test
    public void testDelete() {
        given(this.tagMapper.deleteByIds(any())).willReturn(1);
        int cnt = tagService.delete(Lists.newArrayList("11111"));
        assertEquals(cnt, 1);
    }

    @Test
    public void testFindById() {
        given(this.tagMapper.selectByPrimaryKey(any())).willReturn(buildTagDO());
        TagVO tagVO = tagService.findById("123");
        assertTrue(tagVO != null);
    }

    @Test
    public void testFindByQuery() {
        given(this.tagMapper.selectByQuery(any())).willReturn(Lists.newArrayList(buildTagDO()));
        TagVO tagVO = tagService.findByQuery("film");
        assertTrue(tagVO != null);
    }

    @Test
    public void testFindByParentTagId() {
        given(this.tagMapper.selectByQuery(any())).willReturn(Lists.newArrayList(buildTagDO()));
        List<TagVO> tagVOList = tagService.findByParentTagId("111111");
        assertEquals(tagVOList.size(), 1);
    }

    private TagDTO buildTagDTO() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setId("123");
        tagDTO.setTagDesc("this is a pic tag");
        tagDTO.setParentTagId("111111");
        tagDTO.setExt("test");
        tagDTO.setName("film");
        return tagDTO;
    }

    private TagDO buildTagDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagDO tagDO = TagDO.builder()
                .name("film")
                .tagDesc("this is a pic tag")
                .ext("test")
                .parentTagId("111111")
                .id("123")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagDO;
    }

}
