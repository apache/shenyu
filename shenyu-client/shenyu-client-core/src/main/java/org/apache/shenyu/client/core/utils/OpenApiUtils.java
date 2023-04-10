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

import cn.hutool.core.net.url.UrlPath;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import cn.hutool.core.net.url.UrlQuery;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * openApiUtils.
 */
public class OpenApiUtils {

    private static final String EVERY_PATH = "**";

    private static final String LEFT_ANGLE_BRACKETS = "{";

    private static final String RIGHT_ANGLE_BRACKETS = "}";

    private static final String QUESTION_MARK = "?";

    /**
     * generateDocumentParameters.
     *
     * @param path the api path
     * @return documentParameters
     */
    public static List<Parameter> generateDocumentParameters(final String path) {
        ArrayList<Parameter> list = new ArrayList<>();
        if (isQuery(path)) {
            UrlQuery of = UrlQuery.of(path, Charset.defaultCharset());
            Map<CharSequence, CharSequence> queryMap = of.getQueryMap();
            queryMap.forEach((k, v) -> {
                Parameter parameter = new Parameter();
                parameter.setIn("query");
                parameter.setRequired(false);
                parameter.setName(String.valueOf(k));
                parameter.setSchema(new Schema("string", null));
                list.add(parameter);
            });
        } else {
            Parameter parameter = new Parameter();
            parameter.setIn("path");
            parameter.setName(getPathName(path));
            parameter.setRequired(true);
            parameter.setSchema(new Schema("string", null));
            list.add(parameter);
        }
        return list;
    }

    private static String getPathName(final String path) {
        UrlPath urlPath = UrlPath.of(path, Charset.defaultCharset());
        List<String> segments = urlPath.getSegments();
        for (String segment : segments) {
            if (EVERY_PATH.equals(segment)) {
                return segment;
            }
            if (segment.startsWith(LEFT_ANGLE_BRACKETS) && segment.endsWith(RIGHT_ANGLE_BRACKETS)) {
                return segment.substring(1, segment.length() - 1);
            }
        }
        return path;
    }

    private static boolean isQuery(final String path) {
        if (StringUtils.contains(path, QUESTION_MARK)) {
            return true;
        }
        return false;
    }

    /**
     * generateDocumentResponse.
     *
     * @param path the api path
     * @return documentResponseMap
     */
    public static Map<String, Object> generateDocumentResponse(final String path) {
        ImmutableMap<Object, Object> contentMap = ImmutableMap.builder()
                .put(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE, ImmutableMap.of("schema", ImmutableMap.of("type", "string")))
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

    public static class Parameter {

        private String name;

        private String in;

        private String description;

        private boolean required;

        private Schema schema;

        /**
         * get name.
         *
         * @return name
         */
        public String getName() {
            return name;
        }

        /**
         * set name.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * get in.
         *
         * @return in
         */
        public String getIn() {
            return in;
        }

        /**
         * set in.
         *
         * @param in in
         */
        public void setIn(final String in) {
            this.in = in;
        }

        /**
         * get description.
         *
         * @return description
         */
        public String getDescription() {
            return description;
        }

        /**
         * set description.
         *
         * @param description description
         */
        public void setDescription(final String description) {
            this.description = description;
        }

        /**
         * get required.
         *
         * @return required
         */
        public boolean isRequired() {
            return required;
        }

        /**
         * set required.
         *
         * @param required required
         */
        public void setRequired(final boolean required) {
            this.required = required;
        }

        /**
         * get schema.
         *
         * @return schema
         */
        public Schema getSchema() {
            return schema;
        }

        /**
         * set schema.
         *
         * @param schema schema
         */
        public void setSchema(final Schema schema) {
            this.schema = schema;
        }
    }

    public static class Schema {

        private String type;

        private String format;

        public Schema(final String type, final String format) {
            this.type = type;
            this.format = format;
        }

        /**
         * get type.
         *
         * @return type
         */
        public String getType() {
            return type;
        }

        /**
         * set type.
         *
         * @param type type
         */
        public void setType(final String type) {
            this.type = type;
        }

        /**
         * get format.
         *
         * @return format
         */
        public String getFormat() {
            return format;
        }

        /**
         * set format.
         *
         * @param format format
         */
        public void setFormat(final String format) {
            this.format = format;
        }
    }

}
