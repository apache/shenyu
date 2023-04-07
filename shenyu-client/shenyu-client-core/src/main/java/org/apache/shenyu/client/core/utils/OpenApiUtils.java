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

package org.apache.shenyu.client.core.utils;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * openApiUtils.
 */
public class OpenApiUtils {

    public static class Parameter{

        private String name;

        private String in;

        private String description;

        private boolean required;

        private Schema schema;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getIn() {
            return in;
        }

        public void setIn(String in) {
            this.in = in;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public boolean isRequired() {
            return required;
        }

        public void setRequired(boolean required) {
            this.required = required;
        }

        public Schema getSchema() {
            return schema;
        }

        public void setSchema(Schema schema) {
            this.schema = schema;
        }
    }

    public static class Schema{

        private String type;

        private String format;

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getFormat() {
            return format;
        }

        public void setFormat(String format) {
            this.format = format;
        }
    }

    public static List<Parameter> generateDocumentParameters(final String path) {
        ArrayList<Parameter> list = new ArrayList<>();
        Parameter parameter = new Parameter();
        list.add(parameter);
        return list;
    }

    /**
     * generateDocumentResponse.
     * @param path the api path
     * @return documentResponseMap
     */
    public static Map<String,Object> generateDocumentResponse(final String path){
        ImmutableMap<Object, Object> contentMap = ImmutableMap.builder()
                .put(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE, ImmutableMap.of("schema",ImmutableMap.of("type","string")))
                .build();
        ImmutableMap<Object, Object> successMap = ImmutableMap.builder()
                .put("description", path)
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> notFoundMap = ImmutableMap.builder()
                .put("description", StringUtils.join("the path [", path, "] not found"))
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> conflictMap = ImmutableMap.builder()
                .put("description", "conflict")
                .put("content", contentMap).build();
        return ImmutableMap.<String, Object>builder()
                .put("200", successMap)
                .put("404", notFoundMap)
                .put("409", conflictMap)
                .build();
    }

}
