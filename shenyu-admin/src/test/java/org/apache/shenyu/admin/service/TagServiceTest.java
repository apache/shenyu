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
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
        List<TagDO> list = new ArrayList<>();
        list.add(buildTagDO());
        list.add(buildParentTagDO());
        list.add(buildParentTagDO1());
        list.add(buildParentTagDO2());
        given(this.tagMapper.insert(any())).willReturn(1);
        given(this.tagMapper.selectByPrimaryKey("123")).willReturn(buildTagDO());
        given(this.tagMapper.selectByPrimaryKey("456")).willReturn(buildParentTagDO());
        given(this.tagMapper.selectByPrimaryKey("789")).willReturn(buildParentTagDO1());
        given(this.tagMapper.selectByPrimaryKey("101112")).willReturn(buildParentTagDO2());
        given(this.tagMapper.selectByQuery(any())).willReturn(list);
        tagService.create(buildParentTagDTO());
        tagService.create(buildParentTagDTO1());
        tagService.create(buildTagDTO());
        int cnt = tagService.create(buildTagDTO1());
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
        assertNotNull(tagVO);
    }

    @Test
    public void testFindByQuery() {
        given(this.tagMapper.selectByQuery(any())).willReturn(Lists.newArrayList(buildTagDO()));
        List<TagVO> tagVOS = tagService.findByQuery("film");
        assertEquals(tagVOS.size(), 1);
    }

    @Test
    public void testFindByParentTagId() {
        given(this.tagMapper.selectByQuery(any())).willReturn(Lists.newArrayList(buildTagDO()));
        List<TagVO> tagVOList = tagService.findByParentTagId("111111");
        assertEquals(tagVOList.size(), 1);
    }

    private TagDTO buildTagDTO() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setId("789");
        tagDTO.setTagDesc("this is a pic ta1g");
        tagDTO.setParentTagId("456");
        tagDTO.setName("film1");
        return tagDTO;
    }

    private TagDTO buildTagDTO1() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setId("101112");
        tagDTO.setTagDesc("this is a pic ta1g");
        tagDTO.setParentTagId("789");
        tagDTO.setName("film1");
        return tagDTO;
    }

    private TagDTO buildParentTagDTO1() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setId("456");
        tagDTO.setTagDesc("this is a pic tag");
        tagDTO.setParentTagId("123");
        tagDTO.setName("film");
        return tagDTO;
    }

    private TagDTO buildParentTagDTO() {
        TagDTO tagDTO = new TagDTO();
        tagDTO.setId("123");
        tagDTO.setTagDesc("this is a pic tag");
        tagDTO.setParentTagId("0");
        tagDTO.setName("film");
        return tagDTO;
    }

    private TagDO buildTagDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagDO tagDO = TagDO.builder()
                .name("film")
                .tagDesc("this is a pic tag")
                .parentTagId("0")
                .id("123")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagDO;
    }

    private TagDO buildParentTagDO() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagDO tagDO = TagDO.builder()
                .name("film")
                .tagDesc("this is a pic tag")
                .parentTagId("123")
                .id("456")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagDO;
    }

    private TagDO buildParentTagDO1() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagDO tagDO = TagDO.builder()
                .name("film")
                .tagDesc("this is a pic tag789")
                .parentTagId("456")
                .id("789")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagDO;
    }

    private TagDO buildParentTagDO2() {
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        TagDO tagDO = TagDO.builder()
                .name("film")
                .tagDesc("this is a pic tag789")
                .parentTagId("789")
                .id("101112")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        return tagDO;
    }

}
