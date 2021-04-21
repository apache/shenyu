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

package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.model.dto.DataPermissionDTO;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.DataPermissionQuery;
import org.dromara.soul.admin.model.result.SoulAdminResult;
import org.dromara.soul.admin.model.vo.DataPermissionPageVO;
import org.dromara.soul.admin.service.DataPermissionService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;


/**
 * this is dataPermission controller.
 *
 * @author kaitoShy
 */
@RestController
@RequestMapping("/data-permission")
public class DataPermissionController {

    private final DataPermissionService dataPermissionService;

    @Autowired(required = false)
    public DataPermissionController(final DataPermissionService dataPermissionService) {
        this.dataPermissionService = dataPermissionService;
    }

    /**
     *  Query selector or rule list.
     * @param currentPage current page
     * @param pageSize page size
     * @param userId user id
     * @return SoulAdminResult
     */
    @GetMapping("")
    public SoulAdminResult listPageDataPermissions(@RequestParam("currentPage") final Integer currentPage,
                                                   @RequestParam("pageSize") final Integer pageSize,
                                                   @PathVariable("userId") final String userId) {
        DataPermissionPageVO dataPermissionPageVO = dataPermissionService.listByPage(new DataPermissionQuery(userId, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success(SoulResultMessage.QUERY_SUCCESS, dataPermissionPageVO);
    }


    /**
     * create data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @PostMapping("")
    public SoulAdminResult save(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> SoulAdminResult.success(SoulResultMessage.SAVE_SUCCESS, dataPermissionService.create(dataPermissionDTO)))
                .orElse(SoulAdminResult.error(SoulResultMessage.SAVE_FAILED));

    }

    /**
     * Delete data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows count
     */
    @DeleteMapping("")
    public SoulAdminResult delete(@RequestBody final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO)
                .map(item -> SoulAdminResult.success(SoulResultMessage.DELETE_SUCCESS, dataPermissionService.delete(dataPermissionDTO)))
                .orElse(SoulAdminResult.error(SoulResultMessage.DELETE_SUCCESS));

    }
}
