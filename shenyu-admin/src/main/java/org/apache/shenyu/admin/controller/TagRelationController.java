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
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.TagRelationService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Validated
@RestController
@RequestMapping("/tag-relation/")
public class TagRelationController {

    private final TagRelationService tagRelationService;

    public TagRelationController(final TagRelationService tagRelationService) {
        this.tagRelationService = tagRelationService;
    }

    /**
     * list tag relation.
     *
     * @param tagId tag tagId.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/tagId/{tagId}")
    public ShenyuAdminResult queryApiByTagId(@PathVariable("tagId") @Valid final String tagId) {
        List<TagRelationDO> tagRelationDOS = Optional.ofNullable(tagRelationService.findByTagId(tagId)).orElse(Lists.newArrayList());
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, tagRelationDOS);
    }

    /**
     * update tag relation.
     *
     * @param id    primary key.
     * @param tagRelationDTO tagRelationDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/id/{id}")
    public ShenyuAdminResult updateTagRelation(@PathVariable("id") @Valid final String id,
                                       @Valid @RequestBody final TagRelationDTO tagRelationDTO) {
        tagRelationDTO.setId(id);
        Integer updateCount = tagRelationService.update(tagRelationDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS, updateCount);
    }

    /**
     * delete tag relation.
     *
     * @param ids primary key.
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batchDelete")
    public ShenyuAdminResult deleteTagRelation(@RequestBody @NotEmpty final List<@NotBlank String> ids) {
        Integer deleteCount = tagRelationService.delete(ids);
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS, deleteCount);
    }
}
