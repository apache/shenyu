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

package org.apache.shenyu.plugin.mock.api;

import org.apache.shenyu.common.utils.JsonUtils;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public final class MockRequest {

    private Map<String, String> headers;

    private String uri;

    private String method;

    private Map<String, String> queries;

    private byte[] body;

    private Object json;

    private Map<String, String> forms;

    private MockRequest() {
    }

    /**
     * Gets headers.
     *
     * @return headers
     */
    public Map<String, String> getHeaders() {
        return headers;
    }

    /**
     * Gets uri.
     *
     * @return uri
     */
    public String getUri() {
        return uri;
    }

    /**
     * Gets Method.
     *
     * @return method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Get queries.
     *
     * @return queries
     */
    public Map<String, String> getQueries() {
        return queries;
    }

    /**
     * Get json-body.
     *
     * @return json body
     */
    public Object getJson() {
        if (Objects.isNull(json)) {
            json = JsonUtils.jsonToMap(new String(body));
        }
        return json;
    }

    /**
     * Gets form-body.
     *
     * @return form-body
     */
    public Map<String, String> getForms() {
        if (Objects.isNull(forms)) {
            Map<String, String> parsedForms = new LinkedHashMap<>();
            if (Objects.nonNull(body) && body.length > 0) {
                String formBody = new String(body, StandardCharsets.UTF_8);
                for (String pair : formBody.split("&")) {
                    if (pair.isEmpty()) {
                        continue;
                    }
                    int separator = pair.indexOf('=');
                    String encodedName = separator < 0 ? pair : pair.substring(0, separator);
                    String encodedValue = separator < 0 ? "" : pair.substring(separator + 1);
                    String name = URLDecoder.decode(encodedName, StandardCharsets.UTF_8);
                    String value = URLDecoder.decode(encodedValue, StandardCharsets.UTF_8);
                    parsedForms.putIfAbsent(name, value);
                }
            }
            forms = parsedForms;
        }
        return forms;
    }

    /**
     * Gets xml-body,not support now.
     *
     * @return xml-body
     */
    public Object getXml() {
        //todo
        throw new UnsupportedOperationException();
    }

    public static final class Builder {

        private Map<String, String> headers;

        private String uri;

        private String method;

        private Map<String, String> queries;

        private byte[] body;

        private Builder() {
        }

        /**
         * class builder.
         *
         * @return Builder
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * build headers.
         *
         * @param headers headers
         * @return this
         */
        public Builder headers(final Map<String, String> headers) {
            this.headers = headers;
            return this;
        }

        /**
         * build uri.
         *
         * @param uri uri
         * @return this
         */
        public Builder uri(final String uri) {
            this.uri = uri;
            return this;
        }

        /**
         * build method.
         *
         * @param method method
         * @return this
         */
        public Builder method(final String method) {
            this.method = method;
            return this;
        }

        /**
         * build queries.
         *
         * @param queries queries
         * @return this
         */
        public Builder queries(final Map<String, String> queries) {
            this.queries = queries;
            return this;
        }

        /**
         * build body.
         *
         * @param body body
         * @return this
         */
        public Builder body(final byte[] body) {
            this.body = body;
            return this;
        }

        /**
         * build new Object.
         *
         * @return mockRequest
         */
        public MockRequest build() {
            MockRequest mockRequest = new MockRequest();
            mockRequest.uri = this.uri;
            mockRequest.method = this.method;
            mockRequest.queries = this.queries;
            mockRequest.headers = this.headers;
            mockRequest.body = this.body;
            return mockRequest;
        }
    }
}
