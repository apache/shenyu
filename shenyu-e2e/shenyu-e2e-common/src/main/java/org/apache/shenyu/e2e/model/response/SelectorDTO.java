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
public class SelectorDTO implements ResourceDTO {
    
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
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
    private Date dateCreated;

    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
    private Date dateUpdated;

    private SelectorDTO() {

    }

    private SelectorDTO(Builder builder) {
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
    public void setId(String id) {
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
    public void setPluginId(String pluginId) {
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
    public void setName(String name) {
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
    public void setMatchMode(int matchMode) {
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
     * set matchModeName
     *
     * @param matchModeName matchModeName
     */
    public void setMatchModeName(String matchModeName) {
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
    public void setType(int type) {
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
    public void setTypeName(String typeName) {
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
    public void setSort(int sort) {
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
    public void setEnabled(boolean enabled) {
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
    public void setLogged(boolean logged) {
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
    public void setContinued(boolean continued) {
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
    public void setHandle(String handle) {
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
    public void setConditionList(List<Condition> conditionList) {
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
    public void setDateCreated(Date dateCreated) {
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
    public void setDateUpdated(Date dateUpdated) {
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

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
        private Date dateCreated;

        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
        private Date dateUpdated;

        private Builder() {

        }

        public SelectorDTO build() {
            return new SelectorDTO(this);
        }

        public Builder id(String id) {
            this.id = id;
            return this;
        }

        public Builder pluginId(String pluginId) {
            this.pluginId = pluginId;
            return this;
        }

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder matchMode(int matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        public Builder matchModeName(String matchModeName) {
            this.matchModeName = matchModeName;
            return this;
        }

        public Builder type(int type) {
            this.type = type;
            return this;
        }

        public Builder typeName(String typeName) {
            this.typeName = typeName;
            return this;
        }

        public Builder sort(int sort) {
            this.sort = sort;
            return this;
        }

        public Builder enabled(boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        public Builder logged(boolean logged) {
            this.logged = logged;
            return this;
        }

        public Builder continued(boolean continued) {
            this.continued = continued;
            return this;
        }

        public Builder handle(String handle) {
            this.handle = handle;
            return this;
        }

        public Builder conditionList(List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }

        public Builder dateCreated(Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        public Builder dateUpdated(Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
    }
}
