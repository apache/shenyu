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

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.handle.RuleHandle;

import java.util.List;

/**
 * Rule data.
 */
public final class RuleData implements ResourceData {
    
    /**
     * id.
     */
    private String id;
    
    /**
     * name.
     */
    private String name;
    
    /**
     * selector id.
     */
    private String selectorId;
    
    @JsonProperty("loged")
    private boolean logged;
    
    /**
     * match mode.
     */
    private MatchMode matchMode;
    
    /**
     * sort.
     */
    private int sort;
    
    @JsonSerialize(using = RuleHandle.Serializer.class)
    private RuleHandle handle;

    /**
     * List of ruleConditions.
     */
    @JsonProperty("ruleConditions")
    private List<Condition> conditionList;
    
    /**
     * enabled or not.
     */
    private boolean enabled;
    
    /**
     * match restful or not.
     */
    private Boolean matchRestful;

    /**
     * namespaceId.
     */
    private String namespaceId;

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private RuleData(final RuleDataBuilder builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.selectorId = builder.selectorId;
        this.logged = builder.logged;
        this.matchMode = builder.matchMode;
        this.sort = builder.sort;
        this.handle = builder.handle;
        this.conditionList = builder.conditionList;
        this.enabled = builder.enabled;
        this.matchRestful = builder.matchRestful;
        this.namespaceId = builder.namespaceId;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static RuleDataBuilder builder() {
        return new RuleDataBuilder();
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get id.
     *
     * @return id
     */
    @Override
    public String getId() {
        return id;
    }

    /**
     * set name.
     *
     * @param name name
     */
    @Override
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get name.
     *
     * @return name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * set selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get selectorId.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * is logged.
     *
     * @return logged
     */
    public boolean isLogged() {
        return logged;
    }

    /**
     * set logged.
     *
     * @param logged logged
     */
    public void setLogged(final boolean logged) {
        this.logged = logged;
    }

    /**
     * set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final MatchMode matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get matchMode.
     *
     * @return matchMode
     */
    public MatchMode getMatchMode() {
        return matchMode;
    }

    /**
     * set sort.
     *
     * @param sort sort
     */
    public void setSort(final int sort) {
        this.sort = sort;
    }

    /**
     * get sort.
     *
     * @return sort
     */
    public int getSort() {
        return sort;
    }

    /**
     * set handle.
     *
     * @param handle handle
     */
    public void setHandle(final RuleHandle handle) {
        this.handle = handle;
    }

    /**
     * get handle.
     *
     * @return handle
     */
    public RuleHandle getHandle() {
        return handle;
    }

    /**
     * set conditionList.
     *
     * @param conditionList conditionList
     */
    public void conditionList(final List<Condition> conditionList) {
        this.conditionList = conditionList;
    }

    /**
     * get conditionList.
     *
     * @return conditionList
     */
    public List<Condition> getConditionList() {
        return conditionList;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * is enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * set matchRestful.
     *
     * @param matchRestful matchRestful
     */
    public void setMatchRestful(final Boolean matchRestful) {
        this.matchRestful = matchRestful;
    }

    /**
     * get matchRestful.
     *
     * @return matchRestful
     */
    public Boolean getMatchRestful() {
        return matchRestful;
    }

    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    /**
     * class builder.
     */
    public static final class RuleDataBuilder {

        private String id;

        private String name;

        private String selectorId;

        @JsonProperty("loged")
        private boolean logged;

        private MatchMode matchMode;

        private int sort;

        @JsonSerialize(using = RuleHandle.Serializer.class)
        private RuleHandle handle;

        @JsonProperty("ruleConditions")
        private List<Condition> conditionList;

        private boolean enabled;

        private Boolean matchRestful;

        private String namespaceId;

        /**
         * no args constructor.
         */
        private RuleDataBuilder() {

        }

        /**
         * build new Object.
         *
         * @return RuleData
         */
        public RuleData build() {
            return new RuleData(this);
        }

        /**
         * build id.
         *
         * @param id id
         * @return this
         */
        public RuleDataBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build name.
         *
         * @param name name
         * @return this
         */
        public RuleDataBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * build selectorId.
         *
         * @param selectorId selectorId
         * @return this
         */
        public RuleDataBuilder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        /**
         * build logged.
         *
         * @param logged logged
         * @return this
         */
        public RuleDataBuilder logged(final boolean logged) {
            this.logged = logged;
            return this;
        }

        /**
         * build matchMode.
         *
         * @param matchMode matchMode
         * @return this
         */
        public RuleDataBuilder matchMode(final MatchMode matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * build sort.
         *
         * @param sort sort
         * @return this
         */
        public RuleDataBuilder sort(final int sort) {
            this.sort = sort;
            return this;
        }

        /**
         * build handle.
         *
         * @param handle handle
         * @return this
         */
        public RuleDataBuilder handle(final RuleHandle handle) {
            this.handle = handle;
            return this;
        }

        /**
         * build conditionList.
         *
         * @param conditionList conditionList
         * @return this
         */
        public RuleDataBuilder conditionList(final List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }

        /**
         * build enabled.
         *
         * @param enabled enabled
         * @return this
         */
        public RuleDataBuilder enabled(final boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build matchRestful.
         *
         * @param matchRestful matchRestful
         * @return this
         */
        public RuleDataBuilder matchRestful(final Boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public RuleDataBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
    }
}
