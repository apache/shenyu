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

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.apache.shenyu.e2e.model.data.Condition;

import java.util.Date;
import java.util.List;

/**
 * SelectorDTO.
 */
public final class SelectorDTO implements ResourceDTO {
    
    private String id;
    
    private String pluginId;
    
    private String name;

    private int matchMode;

    private String matchModeName;

    private int type;

    private String typeName;

    private int sort;

    private boolean enabled;

    @JsonProperty(value = "loged")
    private boolean logged;

    private boolean continued;

    private String handle;

    @JsonProperty("selectorConditions")
    private List<Condition> conditionList;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateCreated;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateUpdated;

    private SelectorDTO() {

    }

    private SelectorDTO(final Builder builder) {
        this.id = builder.id;
        this.pluginId = builder.pluginId;
        this.name = builder.name;
        this.matchMode = builder.matchMode;
        this.matchModeName = builder.matchModeName;
        this.type = builder.type;
        this.typeName = builder.typeName;
        this.sort = builder.sort;
        this.enabled = builder.enabled;
        this.logged = builder.logged;
        this.continued = builder.continued;
        this.handle = builder.handle;
        this.conditionList = builder.conditionList;
        this.dateCreated = builder.dateCreated;
        this.dateUpdated = builder.dateUpdated;
    }

    /**
     * builder.
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
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
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get pluginId.
     *
     * @return pluginId
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * set pluginId.
     *
     * @param pluginId pluginId
     */
    public void setPluginId(final String pluginId) {
        this.pluginId = pluginId;
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
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get matchMode.
     *
     * @return matchMode
     */
    public int getMatchMode() {
        return matchMode;
    }

    /**
     * set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final int matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get matchModeName.
     *
     * @return matchModeName
     */
    public String getMatchModeName() {
        return matchModeName;
    }

    /**
     * set matchModeName.
     *
     * @param matchModeName matchModeName
     */
    public void setMatchModeName(final String matchModeName) {
        this.matchModeName = matchModeName;
    }

    /**
     * get type.
     *
     * @return type
     */
    public int getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final int type) {
        this.type = type;
    }

    /**
     * get typeName.
     *
     * @return typeName
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * set typeName.
     *
     * @param typeName typeName
     */
    public void setTypeName(final String typeName) {
        this.typeName = typeName;
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
     * set sort.
     *
     * @param sort sort
     */
    public void setSort(final int sort) {
        this.sort = sort;
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
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
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
     * get continued.
     *
     * @return continued
     */
    public boolean isContinued() {
        return continued;
    }

    /**
     * set continued.
     *
     * @param continued continued
     */
    public void setContinued(final boolean continued) {
        this.continued = continued;
    }

    /**
     * get handle.
     *
     * @return handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * set handle.
     *
     * @param handle handle
     */
    public void setHandle(final String handle) {
        this.handle = handle;
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
     * set conditionList.
     *
     * @param conditionList conditionList
     */
    public void setConditionList(final List<Condition> conditionList) {
        this.conditionList = conditionList;
    }

    /**
     * get dateCreated.
     *
     * @return dateCreated
     */
    @Override
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * set dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get dateUpdated.
     *
     * @return dateUpdated
     */
    @Override
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    public static final class Builder {

        private String id;

        private String pluginId;

        private String name;

        private int matchMode;

        private String matchModeName;

        private int type;

        private String typeName;

        private int sort;

        private boolean enabled;

        @JsonProperty(value = "loged")
        private boolean logged;

        private boolean continued;

        private String handle;

        @JsonProperty("selectorConditions")
        private List<Condition> conditionList;

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
        private Date dateCreated;

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
        private Date dateUpdated;

        private Builder() {

        }

        /**
         * build.
         * @return SelectorDTO
         */
        public SelectorDTO build() {
            return new SelectorDTO(this);
        }

        /**
         * id.
         * @param id id
         * @return Builder
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * plugin id.
         * @param pluginId pluginId
         * @return Builder
         */
        public Builder pluginId(final String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        /**
         * name.
         * @param name name
         * @return Builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * match mode.
         * @param matchMode matchMode
         * @return Builder
         */
        public Builder matchMode(final int matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * match mode name.
         * @param matchModeName matchModeName
         * @return Builder
         */
        public Builder matchModeName(final String matchModeName) {
            this.matchModeName = matchModeName;
            return this;
        }

        /**
         * type.
         * @param type type
         * @return Builder
         */
        public Builder type(final int type) {
            this.type = type;
            return this;
        }

        /**
         * type name.
         * @param typeName typeName
         * @return Builder
         */
        public Builder typeName(final String typeName) {
            this.typeName = typeName;
            return this;
        }

        /**
         * sort.
         * @param sort sort
         * @return Builder
         */
        public Builder sort(final int sort) {
            this.sort = sort;
            return this;
        }
        
        /**
         * enabled.
         * @param enabled enabled
         * @return Builder
         */
        public Builder enabled(final boolean enabled) {
            this.enabled = enabled;
            return this;
        }
        
        /**
         * logged.
         * @param logged logged
         * @return Builder
         */
        public Builder logged(final boolean logged) {
            this.logged = logged;
            return this;
        }
        
        /**
         * continued.
         * @param continued continued
         * @return Builder
         */
        public Builder continued(final boolean continued) {
            this.continued = continued;
            return this;
        }
        
        /**
         * handle.
         * @param handle handle
         * @return Builder
         */
        public Builder handle(final String handle) {
            this.handle = handle;
            return this;
        }
        
        /**
         * condition list.
         * @param conditionList conditionList
         * @return Builder
         */
        public Builder conditionList(final List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }
        
        /**
         * create date.
         * @param dateCreated dateCreated
         * @return Builder
         */
        public Builder dateCreated(final Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }
        
        /**
         * update date.
         * @param dateUpdated dateUpdated
         * @return Builder
         */
        public Builder dateUpdated(final Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
    }
}
