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

import java.util.Arrays;

/**
 * rule query condition.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public final class RuleQueryCondition implements QueryCondition {
    
    /**
     * excluded.
     */
    private String excluded;
    
    /**
     * keyword.
     */
    private String keyword;
    
    /**
     * selectors.
     */
    private String[] selectors;
    
    /**
     * status switch.
     */
    private boolean switchStatus;

    /**
     * status switch.
     */
    private String namespaceId;
    
    /**
     * builder constructor.
     * @param builder builder
     */
    private RuleQueryCondition(final Builder builder) {
        this.excluded = builder.excluded;
        this.keyword = builder.keyword;
        this.selectors = builder.selectors;
        this.switchStatus = builder.switchStatus;
        this.namespaceId = builder.namespaceId;
    }
    
    /**
     * class builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * get excluded.
     *
     * @return excluded
     */
    public String getExcluded() {
        return excluded;
    }
    
    /**
     * set excluded.
     *
     * @param excluded excluded
     */
    public void setExcluded(final String excluded) {
        this.excluded = excluded;
    }
    
    /**
     * get keyword.
     *
     * @return keyword
     */
    public String getKeyword() {
        return keyword;
    }
    
    /**
     * set keyword.
     *
     * @param keyword keyword
     */
    public void setKeyword(final String keyword) {
        this.keyword = keyword;
    }
    
    /**
     * get selectors.
     *
     * @return selectors
     */
    public String[] getSelectors() {
        return selectors;
    }
    
    /**
     * set selectors.
     *
     * @param selectors selectors
     */
    public void setSelectors(final String[] selectors) {
        this.selectors = selectors;
    }
    
    /**
     * is switchStatus.
     *
     * @return switchStatus
     */
    public boolean isSwitchStatus() {
        return switchStatus;
    }
    
    /**
     * set switchStatus.
     *
     * @param switchStatus switchStatus
     */
    public void setSwitchStatus(final boolean switchStatus) {
        this.switchStatus = switchStatus;
    }
    
    @Override
    public String toString() {
        return "RuleQueryCondition{"
                + "excluded='"
                + excluded
                + '\''
                + ", keyword='"
                + keyword
                + '\''
                + ", selectors="
                + Arrays.toString(selectors)
                + ", switchStatus="
                + switchStatus
                + ", namespaceId="
                + namespaceId
                + '}';
    }
    
    /**
     * class builder.
     */
    public static final class Builder {
        
        private String excluded;
        
        private String keyword;
        
        private String[] selectors;
        
        private boolean switchStatus;

        private String namespaceId;
        
        /**
         * no args constructor.
         */
        private Builder() {
        
        }
        
        /**
         * build new Object.
         *
         * @return RuleQueryCondition
         */
        public RuleQueryCondition build() {
            return new RuleQueryCondition(this);
        }
        
        /**
         * build excluded.
         *
         * @param excluded excluded
         * @return this
         */
        public Builder excluded(final String excluded) {
            this.excluded = excluded;
            return this;
        }
        
        /**
         * build keyword.
         *
         * @param keyword keyword
         * @return this
         */
        public Builder keyword(final String keyword) {
            this.keyword = keyword;
            return this;
        }
        
        /**
         * build selectors.
         *
         * @param selectors selectors
         * @return this
         */
        public Builder selectors(final String[] selectors) {
            this.selectors = selectors;
            return this;
        }
        
        /**
         * build switchStatus.
         *
         * @param switchStatus switchStatus
         * @return this
         */
        public Builder switchStatus(final boolean switchStatus) {
            this.switchStatus = switchStatus;
            return this;
        }

        /**
         * namespaceId.
         *
         * @param namespaceId namespaceId
         * @return SelectorDOBuilder
         */
        public Builder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
    }
}
