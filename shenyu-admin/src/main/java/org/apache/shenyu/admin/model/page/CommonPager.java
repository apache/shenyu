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
import java.util.List;
import java.util.Objects;

/**
 * common Pager.
 */
public class CommonPager<T> implements Serializable {

    private static final long serialVersionUID = -1220101004792874251L;

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<T> dataList;

    public CommonPager() {
    }

    public CommonPager(final PageParameter page, final List<T> dataList) {
        this.page = page;
        this.dataList = dataList;
    }

    /**
     * Gets the value of page.
     *
     * @return the value of page
     */
    public PageParameter getPage() {
        return page;
    }

    /**
     * Sets the page.
     *
     * @param page page
     */
    public void setPage(final PageParameter page) {
        this.page = page;
    }

    /**
     * Gets the value of dataList.
     *
     * @return the value of dataList
     */
    public List<T> getDataList() {
        return dataList;
    }

    /**
     * Sets the dataList.
     *
     * @param dataList dataList
     */
    public void setDataList(final List<T> dataList) {
        this.dataList = dataList;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static CommonPager.CommonPagerBuilder builder() {
        return new CommonPager.CommonPagerBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof CommonPager)) {
            return false;
        }
        CommonPager<?> that = (CommonPager<?>) o;
        return Objects.equals(page, that.page) && Objects.equals(dataList, that.dataList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(page, dataList);
    }

    @Override
    public String toString() {
        return "CommonPager{"
                + "page=" + page
                + ", dataList=" + dataList
                + '}';
    }

    public static final class CommonPagerBuilder<T> {

        private PageParameter page;

        private List<T> dataList;

        private CommonPagerBuilder() {
        }

        /**
         * page.
         *
         * @param page the page.
         * @return CommonPagerBuilder.
         */
        public CommonPagerBuilder page(final PageParameter page) {
            this.page = page;
            return this;
        }

        /**
         * dataList.
         *
         * @param dataList the dataList.
         * @return CommonPagerBuilder.
         */
        public CommonPagerBuilder dataList(final List<T> dataList) {
            this.dataList = dataList;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public CommonPager build() {
            CommonPager commonPager = new CommonPager();
            commonPager.setPage(page);
            commonPager.setDataList(dataList);
            return commonPager;
        }
    }
}
