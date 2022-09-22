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

package org.apache.shenyu.e2e.client.admin.model.handle;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.type.ReferenceType;
import com.fasterxml.jackson.databind.type.SimpleType;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.databind.util.Converter;
import lombok.Builder;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.data.RuleData;

@Data
@Builder(toBuilder = true)
public class DivideRuleHandle implements RuleHandle {
    
    private String loadBalance; // todo enhancement, change to enum
    
    private String retryStrategy; // todo enhancement, change to enum
    
    @JsonSerialize(converter = IntConverter.class)
    private int retry;
    
    private long timeout;
    
    private long headerMaxSize;
    
    private long requestMaxSize;
    
    static class IntConverter implements Converter<Integer, String> {
        private static final JavaType _IN = SimpleType.constructUnsafe(int.class);
        private static final JavaType _OUT = ReferenceType.constructUnsafe(String.class);
        
        @Override
        public String convert(Integer integer) {
            return String.valueOf(integer);
        }
        
        @Override
        public JavaType getInputType(TypeFactory typeFactory) {
            return _IN;
        }
        
        @Override
        public JavaType getOutputType(TypeFactory typeFactory) {
            return _OUT;
        }
    }
    
}
