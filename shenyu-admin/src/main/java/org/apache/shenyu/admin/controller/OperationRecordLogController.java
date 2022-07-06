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

import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.apache.shenyu.admin.model.result.AdminResult;
import org.apache.shenyu.admin.service.OperationRecordLogService;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.utils.ResultUtil;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Date;
import java.util.List;

/**
 * OperationRecordLogController.
 */
@Validated
@RestController
@RequestMapping("/operation-record/log")
public class OperationRecordLogController implements PagedController<RecordLogQueryCondition, OperationRecordLog> {
    
    private final OperationRecordLogService recordLogService;
    
    public OperationRecordLogController(final OperationRecordLogService recordLogService) {
        this.recordLogService = recordLogService;
    }
    
    /**
     * list.
     *
     * @return list
     */
    @GetMapping("/list")
    public AdminResult<List<OperationRecordLog>> list() {
        return ResultUtil.ok(recordLogService.list());
    }
    
    /**
     * clean.
     *
     * @param timePoint before time point
     * @return list
     */
    @DeleteMapping("/clean/{timePoint}")
    @RequiresPermissions("system:role:delete")
    public AdminResult<Boolean> clean(@PathVariable @DateTimeFormat(pattern = DateUtils.DATE_FORMAT_DATETIME) final Date timePoint) {
        return ResultUtil.ok(recordLogService.cleanHistory(timePoint));
    }
    
    @Override
    public PageService<RecordLogQueryCondition, OperationRecordLog> pageService() {
        return recordLogService;
    }
}
