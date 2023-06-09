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

package org.apache.shenyu.admin.model.page;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * page condition.
 */
public class PageCondition<T> {
    
    /**
     * current page num.
     */
    @NotNull
    private Integer pageNum;
    
    /**
     * page size.
     */
    @NotNull
    @Max(value = 1000, message = "size max support is 1000")
    @Min(value = 1, message = "size min support is 1")
    private Integer pageSize;
    
    /**
     * condition.
     */
    @Valid
    @NotNull
    private T condition;
    
    public PageCondition() {
    }
    
    public PageCondition(final Integer pageNum, final Integer pageSize, final T condition) {
        this.pageNum = pageNum;
        this.pageSize = pageSize;
        this.condition = condition;
    }
    
    /**
     * get page num.
     *
     * @return page num
     */
    public Integer getPageNum() {
        return pageNum;
    }
    
    /**
     * set page num.
     *
     * @param pageNum page num
     */
    public void setPageNum(final Integer pageNum) {
        this.pageNum = pageNum;
    }
    
    /**
     * get page size.
     *
     * @return page size
     */
    public Integer getPageSize() {
        return pageSize;
    }
    
    /**
     * page size.
     *
     * @param pageSize page size
     */
    public void setPageSize(final Integer pageSize) {
        this.pageSize = pageSize;
    }
    
    /**
     * get condition.
     *
     * @return condition
     */
    public T getCondition() {
        return condition;
    }
    
    /**
     * set condition.
     *
     * @param condition condition
     */
    public void setCondition(final T condition) {
        this.condition = condition;
    }
}
