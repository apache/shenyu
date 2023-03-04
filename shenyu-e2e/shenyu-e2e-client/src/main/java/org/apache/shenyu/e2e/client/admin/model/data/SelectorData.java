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

package org.apache.shenyu.e2e.client.admin.model.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.base.Strings;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.MatchMode;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.SelectorType;
import org.apache.shenyu.e2e.client.admin.model.handle.PluginHandle;

import java.io.IOException;
import java.util.List;

@Data
public class SelectorData implements ResourceData {
    
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
    
    
    
    static class PluginHandleSerializer extends JsonSerializer<PluginHandle> {
        private static final ObjectMapper mapper = new ObjectMapper();
        
        @Override
        public void serialize(PluginHandle pluginHandle, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
            String content = mapper.writer().writeValueAsString(pluginHandle);
            jsonGenerator.writeString(Strings.nullToEmpty(content));
        }
    }
    
    
    SelectorData(String id, String name, Plugin plugin, SelectorType type, MatchMode matchMode, boolean enabled, boolean logged, boolean continued, PluginHandle handle, List<Condition> conditionList, int sort, boolean matchRestful) {
        this.id = id;
        this.name = name;
        this.plugin = plugin;
        this.type = type;
        this.matchMode = matchMode;
        this.enabled = enabled;
        this.logged = logged;
        this.continued = continued;
        this.handle = handle;
        this.conditionList = conditionList;
        this.sort = sort;
        this.matchRestful = matchRestful;
    }
    
    public static SelectorDataBuilder builder() {
        return new SelectorDataBuilder();
    }
    
    public SelectorDataBuilder toBuilder() {
        return (new SelectorDataBuilder()).id(this.id).name(this.name).plugin(this.plugin).type(this.type).matchMode(this.matchMode).enabled(this.enabled).logged(this.logged).continued(this.continued).handle(this.handle).conditionList(this.conditionList).sort(this.sort).matchRestful(this.matchRestful);
    }
    
    /**
     * selector construct.
     *
     * @param builder builder
     */
    private SelectorData(final SelectorDataBuilder builder) {
        setId(builder.id);
        setName(builder.name);
        setPlugin(builder.plugin);
        setType(builder.type);
        setMatchMode(builder.matchMode);
        setEnabled(builder.enabled);
        setLogged(builder.logged);
        setContinued(builder.continued);
        setHandle(builder.handle);
        setConditionList(builder.conditionList);
        setSort(builder.sort);
        setMatchRestful(builder.matchRestful);
    }
    
    public static class SelectorDataBuilder {
        private String id;
        private String name;
        private Plugin plugin;
        private SelectorType type;
        private MatchMode matchMode;
        private boolean enabled;
        private boolean logged;
        private boolean continued;
        private PluginHandle handle;
        private List<Condition> conditionList;
        private int sort;
        private boolean matchRestful;
        
        SelectorDataBuilder() {
        }
        
        public SelectorDataBuilder id(String id) {
            this.id = id;
            return this;
        }
        
        public SelectorDataBuilder name(String name) {
            this.name = name;
            return this;
        }
        
        @JsonProperty("pluginId")
        public SelectorDataBuilder plugin(Plugin plugin) {
            this.plugin = plugin;
            return this;
        }
        
        public SelectorDataBuilder type(SelectorType type) {
            this.type = type;
            return this;
        }
        
        public SelectorDataBuilder matchMode(MatchMode matchMode) {
            this.matchMode = matchMode;
            return this;
        }
        
        public SelectorDataBuilder enabled(boolean enabled) {
            this.enabled = enabled;
            return this;
        }
        
        @JsonProperty("loged")
        public SelectorDataBuilder logged(boolean logged) {
            this.logged = logged;
            return this;
        }
        
        public SelectorDataBuilder continued(boolean continued) {
            this.continued = continued;
            return this;
        }
        
        public SelectorDataBuilder handle(PluginHandle handle) {
            this.handle = handle;
            return this;
        }
        
        @JsonProperty("selectorConditions")
        public SelectorDataBuilder conditionList(List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }
        
        public SelectorDataBuilder sort(int sort) {
            this.sort = sort;
            return this;
        }
        
        public SelectorDataBuilder matchRestful(boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }
        
        public SelectorData build() {
            return new SelectorData(this.id, this.name, this.plugin, this.type, this.matchMode, this.enabled, this.logged, this.continued, this.handle, this.conditionList, this.sort, this.matchRestful);
        }
        
        public String toString() {
            return "SelectorData.SelectorDataBuilder(id=" + this.id + ", name=" + this.name + ", plugin=" + this.plugin + ", type=" + this.type + ", matchMode=" + this.matchMode + ", enabled=" + this.enabled + ", logged=" + this.logged + ", continued=" + this.continued + ", handle=" + this.handle + ", conditionList=" + this.conditionList + ", sort=" + this.sort + ", matchRestful=" + this.matchRestful + ")";
        }
    }
}
