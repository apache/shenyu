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

package org.apache.shenyu.admin.controller;

import com.google.common.collect.Lists;
import java.util.List;
import java.util.Optional;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Validated
@RestController
@RequestMapping("/tag")
public class TagController {

    private final TagService tagService;

    public TagController(final TagService tagService) {
        this.tagService = tagService;
    }

    /**
     * create tag.
     *
     * @param tagDTO tagDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createTag(@Valid @RequestBody final TagDTO tagDTO) {
        Integer createCount = tagService.create(tagDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS, createCount);
    }

    /**
     * query root tag.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/queryRootTag")
    public ShenyuAdminResult queryRootTag() {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, tagService.findByParentTagId("0"));
    }

    /**
     * detail tag.
     *
     * @param id tag name.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/id/{id}")
    public ShenyuAdminResult queryById(@PathVariable("id") @Valid
                                        @Existed(provider = TagMapper.class,
                                                message = "tag is not existed") final String id) {
        TagVO tagVO = tagService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, tagVO);
    }

    /**
     * querylist by parentTagId.
     *
     * @param parentTagId  parentTagId.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/parentTagId/{parentTagId}")
    public ShenyuAdminResult queryListByParentTagId(@PathVariable("parentTagId") @Valid final String parentTagId) {
        List<TagVO> tagVOList = Optional.ofNullable(tagService.findByParentTagId(parentTagId)).orElse(Lists.newArrayList());
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, tagVOList);
    }

    /**
     * detail tag.
     *
     * @param name tag name.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/name/{name}")
    public ShenyuAdminResult queryByName(@PathVariable("name") @Valid final String name) {
        List<TagVO> tagVO = tagService.findByQuery(name);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, tagVO);
    }

    /**
     * update tag.
     *
     * @param id    primary key.
     * @param tagDTO rule.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/id/{id}")
    public ShenyuAdminResult updateTag(@PathVariable("id") @Valid final String id,
                                        @Valid @RequestBody final TagDTO tagDTO) {
        tagDTO.setId(id);
        Integer updateCount = tagService.update(tagDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * delete tags.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batchDelete")
    public ShenyuAdminResult deleteTags(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = tagService.delete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
