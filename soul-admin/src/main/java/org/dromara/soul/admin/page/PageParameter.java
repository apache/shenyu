/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.page;

import lombok.Data;

import java.io.Serializable;

/**
 * PageParameter.
 *
 * @author xiaoyu(Myth)
 */
@Data
public class PageParameter implements Serializable {

    private static final long serialVersionUID = -8324693985921606090L;

    private static final int DEFAULT_PAGE_SIZE = 10;

    private int pageSize;

    private int currentPage;

    private int prePage;

    private int nextPage;

    private int totalPage;

    private int totalCount;

    public PageParameter() {
        this.currentPage = 1;
        this.pageSize = DEFAULT_PAGE_SIZE;
    }

    /**
     * PageParameter.
     *
     * @param currentPage current page.
     * @param pageSize    page size.
     */
    public PageParameter(final int currentPage, final int pageSize) {
        this.currentPage = currentPage;
        this.pageSize = pageSize;
    }

    /**
     * get current page.
     *
     * @return current page
     */
    public int getCurrentPage() {
        return currentPage;
    }
}
