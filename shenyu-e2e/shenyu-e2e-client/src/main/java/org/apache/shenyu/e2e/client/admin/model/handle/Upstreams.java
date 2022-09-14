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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Lists;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.handle.Upstreams.Serializer;

import java.io.IOException;
import java.util.List;

@JsonSerialize(using = Serializer.class)
public class Upstreams implements PluginHandle {
    
    private List<Upstream> upstreams;
    
    Upstreams(List<Upstream> upstreams) {
        this.upstreams = upstreams;
    }
    
    public List<Upstream> getUpstreams() {
        return this.upstreams;
    }
    
    public static Builder builder() {
        return new Builder();
    }
    
    @Data
    @lombok.Builder
    public static class Upstream {
        private String upstreamUrl;
    }
    
    public static final class Builder {
        private List<Upstream> upstreams = Lists.newArrayList();
        
        public Builder add(Upstream upstream) {
            upstreams.add(upstream);
            return this;
        }
        
        public Upstreams build() {
            return new Upstreams(this.upstreams);
        }
    }
    
    static class Serializer extends JsonSerializer<Upstreams> {
        private final ObjectMapper mapper = new ObjectMapper();
        
        @Override
        public void serialize(Upstreams upstreams, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
            jsonGenerator.writeRaw(mapper.writer().writeValueAsString(upstreams.upstreams));
        }
    }
}
