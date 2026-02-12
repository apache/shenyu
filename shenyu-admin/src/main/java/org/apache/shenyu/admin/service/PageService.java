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

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageCondition;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import java.util.ArrayList;
import java.util.List;

/**
 * PageService.
 */
public interface PageService<Q, R> {
    
    
    /**
     * search by page condition.
     *
     * @param pageCondition page condition
     * @return list
     */
    default PageInfo<R> searchByPage(final PageCondition<Q> pageCondition) {
        doConditionPreProcessing(pageCondition.getCondition());
        if (useJpaPage()) {
            Page<R> page = jpaSearchByCondition(pageCondition);
            com.github.pagehelper.Page<R> phPage = new com.github.pagehelper.Page<>();
            phPage.addAll(page.getContent());
            phPage.setPageNum(pageCondition.getPageNum());
            phPage.setPageSize(pageCondition.getPageSize());
            phPage.setTotal(page.getTotalElements());
            return new PageInfo<>(phPage);
        }
        PageHelper.startPage(pageCondition.getPageNum(), pageCondition.getPageSize());
        return new PageInfo<>(searchByCondition(pageCondition.getCondition()));
    }
    
    
    /**
     * search by page condition.
     *
     * @param pageCondition page condition
     * @return list
     */
    default CommonPager<R> searchByPageToPager(final PageCondition<Q> pageCondition) {
        final PageInfo<R> pageInfo = searchByPage(pageCondition);
        return new CommonPager<>(new PageParameter(pageCondition.getPageNum(), pageCondition.getPageSize(), (int) pageInfo.getTotal()), pageInfo.getList());
    }
    
    /**
     * search by condition.
     *
     * @param condition condition
     * @return list
     */
    default List<R> searchByCondition(final Q condition) {
        // default is empty list, if paged used DB query.
        return new ArrayList<>();
    }
    
    /**
     * condition preprocessing.
     *
     * @param condition condition
     */
    default void doConditionPreProcessing(final Q condition) {
        // default is nothing, override condition.
    }

    /**
     * use jpa page.
     *
     * @see PageService#jpaSearchByCondition(PageCondition)
     * @return true if use jpa page
     */
    default boolean useJpaPage() {
        return false;
    }

    /**
     * jpa search by condition.
     *
     * @param pageCondition page condition
     * @return  page
     */
    default Page<R> jpaSearchByCondition(final PageCondition<Q> pageCondition) {
        return new PageImpl<>(new ArrayList<>());
    }
}
