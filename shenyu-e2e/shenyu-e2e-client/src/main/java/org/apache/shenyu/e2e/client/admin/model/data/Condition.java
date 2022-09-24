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

import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Builder;
import lombok.Data;
import org.jetbrains.annotations.NotNull;

@Data
@Builder(toBuilder = true)
public class Condition {
    @NotNull
    private ParamType paramType;
    
    @NotNull
    private Operator operator;
    
    private String paramName;
    
    private String paramValue;
    
    
    public enum ParamType {
        
        POST("post"),
        
        METHOD("req_method"),
    
        URI("uri"),
        QUERY("query"),
        HEADER("header"),
        COOKIE("cookie"),
    
        IP("ip"),
        HOST("host"),
        DOMAIN("domain"),
        ;
    
        private final String alias;
        ParamType(String alias) {
            this.alias = alias;
        }
    
        @JsonValue
        public String getAlias() {
            return alias;
        }
    }
    
    public enum Operator {
        MATCH("match"),
        EQUAL("="),
        REGEX("regex"),
        CONTAINS("contains"),
        TIME_BEFORE("TimeBefore"),
        TIME_AFTER("TimeAfter"),
        EXCLUDE("exclude"),
        STARTS_WITH("startsWith"),
        ENDS_WITH("endsWith"),
        PATH_PATTERN("pathPattern"),
        ;
        
        private final String alias;
        Operator(String alias) {
            this.alias = alias;
        }
    
        @JsonValue
        public String getAlias() {
            return alias;
        }
    }
    
}
