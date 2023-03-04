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
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.MatchMode;
import org.apache.shenyu.e2e.client.admin.model.handle.RuleHandle;
import org.apache.shenyu.e2e.client.admin.model.handle.RuleHandle.Serializer;

import java.util.List;

@Data
public class RuleData implements ResourceData {
    
    private String id;
    
    private String name;
    
    private String selectorId;
    
    @JsonProperty("loged")
    private boolean logged;
    
    private MatchMode matchMode;
    
    private int sort;
    
    @JsonSerialize(using = Serializer.class)
    private RuleHandle handle;
    
    @JsonProperty("ruleConditions")
    private List<Condition> conditionList;
    
    private boolean enabled;
    
    private Boolean matchRestful;
    
    
    RuleData(String id, String name, String selectorId, boolean logged, MatchMode matchMode, int sort, RuleHandle handle, List<Condition> conditionList, boolean enabled, Boolean matchRestful) {
        this.id = id;
        this.name = name;
        this.selectorId = selectorId;
        this.logged = logged;
        this.matchMode = matchMode;
        this.sort = sort;
        this.handle = handle;
        this.conditionList = conditionList;
        this.enabled = enabled;
        this.matchRestful = matchRestful;
    }
    
    public static RuleDataBuilder builder() {
        return new RuleDataBuilder();
    }
    
    public static class RuleDataBuilder {
        private String id;
        private String name;
        private String selectorId;
        private boolean logged;
        private MatchMode matchMode;
        private int sort;
        private RuleHandle handle;
        private List<Condition> conditionList;
        private boolean enabled;
        private Boolean matchRestful;
        
        RuleDataBuilder() {
        }
        
        public RuleDataBuilder id(String id) {
            this.id = id;
            return this;
        }
        
        public RuleDataBuilder name(String name) {
            this.name = name;
            return this;
        }
        
        public RuleDataBuilder selectorId(String selectorId) {
            this.selectorId = selectorId;
            return this;
        }
        
        @JsonProperty("loged")
        public RuleDataBuilder logged(boolean logged) {
            this.logged = logged;
            return this;
        }
        
        public RuleDataBuilder matchMode(MatchMode matchMode) {
            this.matchMode = matchMode;
            return this;
        }
        
        public RuleDataBuilder sort(int sort) {
            this.sort = sort;
            return this;
        }
        
        public RuleDataBuilder handle(RuleHandle handle) {
            this.handle = handle;
            return this;
        }
        
        @JsonProperty("ruleConditions")
        public RuleDataBuilder conditionList(List<Condition> conditionList) {
            this.conditionList = conditionList;
            return this;
        }
        
        public RuleDataBuilder enabled(boolean enabled) {
            this.enabled = enabled;
            return this;
        }
        
        public RuleDataBuilder matchRestful(Boolean matchRestful) {
            this.matchRestful = matchRestful;
            return this;
        }
        
        public RuleData build() {
            return new RuleData(this.id, this.name, this.selectorId, this.logged, this.matchMode, this.sort, this.handle, this.conditionList, this.enabled, this.matchRestful);
        }
        
        public String toString() {
            return "RuleData.RuleDataBuilder(id=" + this.id + ", name=" + this.name + ", selectorId=" + this.selectorId + ", logged=" + this.logged + ", matchMode=" + this.matchMode + ", sort=" + this.sort + ", handle=" + this.handle + ", conditionList=" + this.conditionList + ", enabled=" + this.enabled + ", matchRestful=" + this.matchRestful + ")";
        }
    }
}
