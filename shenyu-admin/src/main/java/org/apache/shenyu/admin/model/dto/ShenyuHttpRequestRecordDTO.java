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

package org.apache.shenyu.admin.model.dto;

import java.util.List;
import java.util.Map;

public class ShenyuHttpRequestRecordDTO {

    private List<Record> records;

    public List<Record> getRecords() {
        return records;
    }

    public void setRecords(final List<Record> records) {
        this.records = records;
    }

    public static class Record {

        private String taskId;

        private String traceId;

        private String method;

        private String requestUri;

        private String queryParams;

        private String requestBody;

        private Map<String, String> requestHeaders;

        private Integer status;

        private Map<String, String> responseHeaders;

        private String responseBody;

        public String getTaskId() {
            return taskId;
        }

        public void setTaskId(final String taskId) {
            this.taskId = taskId;
        }

        public void setRequestBody(final String requestBody) {
            this.requestBody = requestBody;
        }

        public void setMethod(final String method) {
            this.method = method;
        }

        public void setRequestUri(final String requestUri) {
            this.requestUri = requestUri;
        }

        public void setQueryParams(final String queryParams) {
            this.queryParams = queryParams;
        }

        public void setRequestHeaders(final Map<String, String> requestHeaders) {
            this.requestHeaders = requestHeaders;
        }

        public void setStatus(final Integer status) {
            this.status = status;
        }

        public void setResponseHeaders(final Map<String, String> responseHeaders) {
            this.responseHeaders = responseHeaders;
        }

        public void setResponseBody(final String responseBody) {
            this.responseBody = responseBody;
        }

        public void setTraceId(final String traceId) {
            this.traceId = traceId;
        }

        public String getTraceId() {
            return traceId;
        }

        public String getMethod() {
            return method;
        }

        public String getRequestUri() {
            return requestUri;
        }

        public String getQueryParams() {
            return queryParams;
        }

        public Map<String, String> getRequestHeaders() {
            return requestHeaders;
        }

        public Integer getStatus() {
            return status;
        }

        public String getResponseBody() {
            return responseBody;
        }

        public Map<String, String> getResponseHeaders() {
            return responseHeaders;
        }

        public String getRequestBody() {
            return requestBody;
        }

    }

}
