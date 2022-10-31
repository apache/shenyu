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

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import lombok.Builder;
import lombok.Getter;
import lombok.ToString;

@Getter
@Builder
@ToString
@JsonInclude(Include.NON_NULL)
public class SearchCondition {
    
    public interface QueryCondition {
        String getExcluded();
        
        String getKeyword();
        
        boolean isSwitchStatus();
    }
    
    @Getter
    @Builder
    @ToString
    @JsonInclude(Include.NON_NULL)
    public static class SelectorQueryCondition implements QueryCondition {
        private String excluded;
        private String keyword;
        
        @JsonAlias("plugin")
        private String[] plugins;
        
        private boolean switchStatus;
    }
    
    @Getter
    @Builder
    @ToString
    @JsonInclude(Include.NON_NULL)
    public static class RuleQueryCondition implements QueryCondition {
        private String excluded;
        private String keyword;
        
        private String[] selectors;
        
        private boolean switchStatus;
    }
    
    public static QueryCondition QUERY_ALL = new QueryCondition() {
    
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
    
}
