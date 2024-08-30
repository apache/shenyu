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
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.base.Strings;
import org.apache.shenyu.e2e.model.MatchMode;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.SelectorType;
import org.apache.shenyu.e2e.model.handle.PluginHandle;

import java.io.IOException;
import java.util.List;

/**
 * selector data.
 */
public final class SelectorData implements ResourceData {
    
    /**
     * id.
     */
    private String id;
    
    private String name;
    
    @JsonProperty("pluginId")
    private Plugin plugin;
    
    private SelectorType type;
    
    private MatchMode matchMode;
    
    private boolean enabled;
    
    @JsonProperty(value = "loged")
    private boolean logged;
    
    private boolean continued;
    
    @JsonSerialize(using = PluginHandleSerializer.class)
    private PluginHandle handle;
    
    @JsonProperty("selectorConditions")
    private List<Condition> conditionList;
    
    private int sort;
    
    private boolean matchRestful;

    private String namespaceId;

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private SelectorData(final SelectorDataBuilder builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.plugin = builder.plugin;
        this.type = builder.type;
        this.matchMode = builder.matchMode;
        this.enabled = builder.enabled;
        this.logged = builder.logged;
        this.continued = builder.continued;
        this.handle = builder.handle;
        this.conditionList = builder.conditionList;
        this.sort = builder.sort;
        this.matchRestful = builder.matchRestful;
        this.namespaceId = builder.namespaceId;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static SelectorDataBuilder builder() {
        return new SelectorDataBuilder();
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
    @Override
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get plugin.
     *
     * @return plugin
     */
    public Plugin getPlugin() {
        return plugin;
    }

    /**
     * set plugin.
     *
     * @param plugin plugin
     */
    public void plugin(final Plugin plugin) {
        this.plugin = plugin;
    }

    /**
     * get type.
     *
     * @return type
     */
    public SelectorType getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final SelectorType type) {
        this.type = type;
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
     * set matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final MatchMode matchMode) {
        this.matchMode = matchMode;
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
     * is continued.
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
    public PluginHandle getHandle() {
        return handle;
    }

    /**
     * set handle.
     *
     * @param handle handle
     */
    public void handle(final PluginHandle handle) {
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
    public void conditionList(final List<Condition> conditionList) {
        this.conditionList = conditionList;
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
     * is matchRestful.
     *
     * @return matchRestful
     */
    public boolean isMatchRestful() {
        return matchRestful;
    }

    /**
     * set matchRestful.
     *
     * @param matchRestful matchRestful
     */
    public void setMatchRestful(final boolean matchRestful) {
        this.matchRestful = matchRestful;
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

    static class PluginHandleSerializer extends JsonSerializer<PluginHandle> {
        private static final ObjectMapper MAPPER = new ObjectMapper();
        
        @Override
        public void serialize(final PluginHandle pluginHandle, final JsonGenerator jsonGenerator, final SerializerProvider serializerProvider) throws IOException {
            String content = MAPPER.writer().writeValueAsString(pluginHandle);
            jsonGenerator.writeString(Strings.nullToEmpty(content));
        }
    }

    /**
     * class builder.
     */
    public static final class SelectorDataBuilder {

        private String id;

        private String name;

        @JsonProperty("pluginId")
        private Plugin plugin;

        private SelectorType type;

        private MatchMode matchMode;

        private boolean enabled;

        @JsonProperty(value = "loged")
        private boolean logged;

        private boolean continued;

        @JsonSerialize(using = PluginHandleSerializer.class)
        private PluginHandle handle;

        @JsonProperty("selectorConditions")
        private List<Condition> conditionList;

        private int sort;

        private boolean matchRestful;

        private String namespaceId;

        /**
         * no args constructor.
         */
        private SelectorDataBuilder() {

        }

        /**
         * build new Object.
         *
         * @return SelectorData
         */
        public SelectorData build() {
            return new SelectorData(this);
        }

        /**
         * build id.
         *
         * @param id id
         * @return this
         */
        public SelectorDataBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build name.
         *
         * @param name name
         * @return this
         */
        public SelectorDataBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * build plugin.
         *
         * @param plugin plugin
         * @return this
         */
        public SelectorDataBuilder plugin(final Plugin plugin) {
            this.plugin = plugin;
            return this;
        }

        /**
         * build type.
         *
         * @param type type
         * @return this
         */
        public SelectorDataBuilder type(final SelectorType type) {
            this.type = type;
            return this;
        }

        /**
         * build matchMode.
         *
         * @param matchMode matchMode
         * @return this
         */
        public SelectorDataBuilder matchMode(final MatchMode matchMode) {
            this.matchMode = matchMode;
            return this;
        }

        /**
         * build enabled.
         *
         * @param enabled enabled
         * @return this
         */
        public SelectorDataBuilder enabled(final boolean enabled) {
            this.enabled = enabled;
            return this;
        }

        /**
         * build logged.
         *
         * @param logged logged
         * @return this
         */
        public SelectorDataBuilder logged(final boolean logged) {
            this.logged = logged;
            return this;
        }

        /**
         * build continued.
         *
         * @param continued continued
         * @return this
         */
        public SelectorDataBuilder continued(final boolean continued) {
            this.continued = continued;
            return this;
        }

        /**
         * build handle.
         *
         * @param handle handle
         * @return this
         */
        public SelectorDataBuilder handle(final PluginHandle handle) {
            this.handle = handle;
            return this;
        }

        /**
         * build conditionList.
         *
         * @param conditionList conditionList
         * @return this
         */
        public SelectorDataBuilder conditionList(final List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }

        /**
         * build sort.
         *
         * @param sort sort
         * @return this
         */
        public SelectorDataBuilder sort(final int sort) {
            this.sort = sort;
            return this;
        }

        /**
         * build matchRestful.
         *
         * @param matchRestful matchRestful
         * @return this
         */
        public SelectorDataBuilder matchRestful(final boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }

        /**
         * build namespaceId.
         *
         * @param namespaceId namespaceId
         * @return this
         */
        public SelectorDataBuilder namespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
            return this;
        }
    }
}
