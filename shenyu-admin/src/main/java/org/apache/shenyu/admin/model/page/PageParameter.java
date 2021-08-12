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

import java.io.Serializable;
import java.util.Objects;

/**
 * PageParameter.
 */
public class PageParameter implements Serializable {

    private static final long serialVersionUID = -8324693985921606090L;

    private static final int DEFAULT_PAGE_SIZE = 12;

    private int currentPage;

    private int prePage;

    private int nextPage;

    private int pageSize;

    private int offset;

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
    public PageParameter(final Integer currentPage, final Integer pageSize) {
        this.currentPage = currentPage == null || currentPage <= 0 ? 1 : currentPage;
        this.pageSize = pageSize == null || pageSize <= 0 ? DEFAULT_PAGE_SIZE : pageSize;
        this.offset = (this.currentPage - 1) * this.pageSize;
    }

    /**
     * PageParameter.
     *
     * @param currentPage current page.
     * @param pageSize    page size.
     * @param totalCount  total count.
     */
    public PageParameter(final int currentPage, final int pageSize, final int totalCount) {
        this.currentPage = currentPage;
        this.pageSize = pageSize;
        this.totalCount = totalCount;
        this.totalPage = (int) Math.ceil((double) totalCount / (double) pageSize);
        this.prePage = currentPage <= 1 ? 1 : currentPage - 1;
        this.nextPage = currentPage >= this.totalPage ? this.totalPage : currentPage + 1;
    }

    /**
     * Gets the value of currentPage.
     *
     * @return the value of currentPage
     */
    public int getCurrentPage() {
        return currentPage;
    }

    /**
     * Sets the currentPage.
     *
     * @param currentPage currentPage
     */
    public void setCurrentPage(final int currentPage) {
        this.currentPage = currentPage;
    }

    /**
     * Gets the value of prePage.
     *
     * @return the value of prePage
     */
    public int getPrePage() {
        return prePage;
    }

    /**
     * Sets the prePage.
     *
     * @param prePage prePage
     */
    public void setPrePage(final int prePage) {
        this.prePage = prePage;
    }

    /**
     * Gets the value of nextPage.
     *
     * @return the value of nextPage
     */
    public int getNextPage() {
        return nextPage;
    }

    /**
     * Sets the nextPage.
     *
     * @param nextPage nextPage
     */
    public void setNextPage(final int nextPage) {
        this.nextPage = nextPage;
    }

    /**
     * Gets the value of pageSize.
     *
     * @return the value of pageSize
     */
    public int getPageSize() {
        return pageSize;
    }

    /**
     * Sets the pageSize.
     *
     * @param pageSize pageSize
     */
    public void setPageSize(final int pageSize) {
        this.pageSize = pageSize;
    }

    /**
     * Gets the value of offset.
     *
     * @return the value of offset
     */
    public int getOffset() {
        return offset;
    }

    /**
     * Sets the offset.
     *
     * @param offset offset
     */
    public void setOffset(final int offset) {
        this.offset = offset;
    }

    /**
     * Gets the value of totalPage.
     *
     * @return the value of totalPage
     */
    public int getTotalPage() {
        return totalPage;
    }

    /**
     * Sets the totalPage.
     *
     * @param totalPage totalPage
     */
    public void setTotalPage(final int totalPage) {
        this.totalPage = totalPage;
    }

    /**
     * Gets the value of totalCount.
     *
     * @return the value of totalCount
     */
    public int getTotalCount() {
        return totalCount;
    }

    /**
     * Sets the totalCount.
     *
     * @param totalCount totalCount
     */
    public void setTotalCount(final int totalCount) {
        this.totalCount = totalCount;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PageParameter)) {
            return false;
        }
        PageParameter that = (PageParameter) o;
        return currentPage == that.currentPage
                && prePage == that.prePage
                && nextPage == that.nextPage
                && pageSize == that.pageSize
                && offset == that.offset
                && totalPage == that.totalPage
                && totalCount == that.totalCount;
    }

    @Override
    public int hashCode() {
        return Objects.hash(currentPage, prePage, nextPage, pageSize, offset, totalPage, totalCount);
    }

    @Override
    public String toString() {
        return "PageParameter{"
                + "currentPage=" + currentPage
                + ", prePage=" + prePage
                + ", nextPage=" + nextPage
                + ", pageSize=" + pageSize
                + ", offset=" + offset
                + ", totalPage=" + totalPage
                + ", totalCount=" + totalCount
                + '}';
    }
}
