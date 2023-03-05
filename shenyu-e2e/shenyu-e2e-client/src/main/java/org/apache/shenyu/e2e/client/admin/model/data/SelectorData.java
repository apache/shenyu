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
import lombok.Builder;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.MatchMode;
import org.apache.shenyu.e2e.client.admin.model.Plugin;
import org.apache.shenyu.e2e.client.admin.model.SelectorType;
import org.apache.shenyu.e2e.client.admin.model.handle.PluginHandle;

import java.io.IOException;
import java.util.List;

@Data
@Builder(toBuilder = true)
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
    
}
