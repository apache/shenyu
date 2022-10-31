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

import com.github.pagehelper.PageInfo;

import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageCondition;
import org.apache.shenyu.admin.model.result.AdminResult;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.utils.ResultUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * PagedController.
 */
public interface PagedController<V, T> {
    
    /**
     * list - paged query.
     *
     * @param pageCondition page condition
     * @return PageInfo
     */
    @PostMapping("list/search")
    default AdminResult<PageInfo<T>> search(@RequestBody @Validated final PageCondition<V> pageCondition) {
        return ResultUtil.ok(pageService().searchByPage(pageCondition), ShenyuResultMessage.QUERY_SUCCESS);
    }
    
    /**
     * list - paged query-adaptor.
     *
     * @param pageCondition page condition
     * @return CommonPager
     */
    @PostMapping("list/search/adaptor")
    default AdminResult<CommonPager<T>> searchAdaptor(
            @RequestBody @Validated final PageCondition<V> pageCondition) {
        return ResultUtil.ok(pageService().searchByPageToPager(pageCondition), ShenyuResultMessage.QUERY_SUCCESS);
    }
    
    /**
     * page service.
     *
     * @return paged service
     */
    PageService<V, T> pageService();
}
