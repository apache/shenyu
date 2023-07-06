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

package org.apache.shenyu.e2e.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.util.List;

/**
 * Searched resources.
 *
 * @param <T> resourceDTO
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchedResources<T extends ResourceDTO> {
    
    private int pageNum;

    private int pageSize;

    private int total;

    private int pages;
    
    private List<T> list;

    /**
     * get pageNum.
     *
     * @return pageNum
     */
    public int getPageNum() {
        return pageNum;
    }

    /**
     * set pageNum.
     *
     * @param pageNum pageNum
     */
    public void setPageNum(final int pageNum) {
        this.pageNum = pageNum;
    }

    /**
     * get pageSize.
     *
     * @return pageSize
     */
    public int getPageSize() {
        return pageSize;
    }

    /**
     * set pageSize.
     *
     * @param pageSize pageSize
     */
    public void setPageSize(final int pageSize) {
        this.pageSize = pageSize;
    }

    /**
     * get total.
     *
     * @return total
     */
    public int getTotal() {
        return total;
    }

    /**
     * set total.
     *
     * @param total total
     */
    public void setTotal(final int total) {
        this.total = total;
    }

    /**
     * get pages.
     *
     * @return pages
     */
    public int getPages() {
        return pages;
    }

    /**
     * set pages.
     *
     * @param pages pages
     */
    public void setPages(final int pages) {
        this.pages = pages;
    }

    /**
     * get list.
     *
     * @return list
     */
    public List<T> getList() {
        return list;
    }

    /**
     * set list.
     *
     * @param list list
     */
    public void setList(final List<T> list) {
        this.list = list;
    }
}
