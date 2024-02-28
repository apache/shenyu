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

package org.apache.shenyu.e2e.model.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * The condition of searching.
 */
@JsonInclude(Include.NON_NULL)
public final class SearchCondition {
    
    public static final QueryCondition QUERY_ALL = new QueryCondition() {
        
        @Override
        public String getExcluded() {
            return null;
        }
        
        @Override
        public String getKeyword() {
            return null;
        }
    
        @Override
        public boolean isSwitchStatus() {
            return true;
        }
    };
    
    private int pageNum;
    
    private int pageSize;
    
    private QueryCondition condition;
    
    /**
     * builder constructor.
     * @param builder builder
     */
    private SearchCondition(final Builder builder) {
        this.pageNum = builder.pageNum;
        this.pageSize = builder.pageSize;
        this.condition = builder.condition;
    }
    
    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

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
     * get condition.
     *
     * @return condition
     */
    public QueryCondition getCondition() {
        return condition;
    }

    /**
     * set condition.
     *
     * @param condition condition
     */
    public void setCondition(final QueryCondition condition) {
        this.condition = condition;
    }

    @Override
    public String toString() {
        return "SearchCondition{"
                + "pageNum="
                + pageNum
                + ", pageSize="
                + pageSize
                + ", condition="
                + condition
                + '}';
    }

    /**
     * class builder.
     */
    public static final class Builder {

        private int pageNum;

        private int pageSize;

        private QueryCondition condition;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return SearchCondition
         */
        public SearchCondition build() {
            return new SearchCondition(this);
        }

        /**
         * build pageNum.
         *
         * @param pageNum pageNum
         * @return this
         */
        public Builder pageNum(final int pageNum) {
            this.pageNum = pageNum;
            return this;
        }

        /**
         * build pageSize.
         *
         * @param pageSize pageSize
         * @return this
         */
        public Builder pageSize(final int pageSize) {
            this.pageSize = pageSize;
            return this;
        }

        /**
         * build condition.
         *
         * @param condition condition
         * @return this
         */
        public Builder condition(final QueryCondition condition) {
            this.condition = condition;
            return this;
        }
    }
}
