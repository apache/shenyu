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

import java.util.List;

/**
 * Paginated resources.
 *
 * @param <T> resourceDTO
 */
public class PaginatedResources<T extends ResourceDTO> {
    
    private Pagination page;
    
    private List<T> dataList;

    /**
     * get page.
     *
     * @return page
     */
    public Pagination getPage() {
        return page;
    }

    /**
     * set page.
     *
     * @param page page
     */
    public void setPage(final Pagination page) {
        this.page = page;
    }

    /**
     * get dataList.
     *
     * @return dataList
     */
    public List<T> getDataList() {
        return dataList;
    }

    /**
     * set dataList.
     *
     * @param dataList dataList
     */
    public void setDataList(final List<T> dataList) {
        this.dataList = dataList;
    }
}
