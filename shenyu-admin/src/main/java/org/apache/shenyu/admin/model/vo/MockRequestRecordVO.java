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

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;

/**
 * MockRequestRecordVO.
 */
public class MockRequestRecordVO implements Serializable {
    private static final long serialVersionUID = -7675972300371815619L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * the api id.
     */
    private String apiId;

    /**
     * the request host.
     */
    private String host;

    /**
     * the request port.
     */
    private Integer port;

    /**
     * the request url.
     */
    private String url;

    /**
     * the request param in url.
     */
    private String pathVariable;

    /**
     * the request param after url.
     */
    private String query;

    /**
     * the request param in header.
     */
    private String header;

    /**
     * the request body.
     */
    private String body;

    /**
     * dateCreated.
     */
    private String dateCreated;

    /**
     * dateUpdated.
     */
    private String dateUpdated;

    public MockRequestRecordVO() {
    }

    public MockRequestRecordVO(final String id, final String apiId, final String host, final Integer port,
                               final String url, final String pathVariable, final String query, final String header,
                               final String body, final String dateCreated, final String dateUpdated) {
        this.id = id;
        this.apiId = apiId;
        this.host = host;
        this.port = port;
        this.url = url;
        this.pathVariable = pathVariable;
        this.query = query;
        this.header = header;
        this.body = body;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

    /**
     * getId.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets the value of apiId.
     *
     * @return the value of apiId
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * Sets the apiId.
     *
     * @param apiId apiId
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * Gets the value of host.
     *
     * @return the value of host
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets the host.
     *
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * Gets the value of port.
     *
     * @return the value of port
     */
    public Integer getPort() {
        return port;
    }

    /**
     * Sets the port.
     *
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }

    /**
     * Gets the value of url.
     *
     * @return the value of url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets the url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets the value of pathVariable.
     *
     * @return the value of pathVariable
     */
    public String getPathVariable() {
        return pathVariable;
    }

    /**
     * Sets the pathVariable.
     *
     * @param pathVariable pathVariable
     */
    public void setPathVariable(final String pathVariable) {
        this.pathVariable = pathVariable;
    }

    /**
     * Gets the value of query.
     *
     * @return the value of query
     */
    public String getQuery() {
        return query;
    }

    /**
     * Sets the query.
     *
     * @param query query
     */
    public void setQuery(final String query) {
        this.query = query;
    }

    /**
     * Gets the value of header.
     *
     * @return the value of header
     */
    public String getHeader() {
        return header;
    }

    /**
     * Sets the header.
     *
     * @param header header
     */
    public void setHeader(final String header) {
        this.header = header;
    }

    /**
     * Gets the value of body.
     *
     * @return the value of body
     */
    public String getBody() {
        return body;
    }

    /**
     * Sets the body.
     *
     * @param body body
     */
    public void setBody(final String body) {
        this.body = body;
    }

    /**
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * builder.
     *
     * @return ApiVOBuilder
     */
    public static MockRequestRecordVO.MockRequestRecordVOBuilder builder() {
        return new MockRequestRecordVO.MockRequestRecordVOBuilder();
    }

    /**
     * buildMockRequestRecordVO.
     * @param mockRequestRecordDO mockRequestRecordDO
     * @return MockRequestRecordVO
     */
    public static MockRequestRecordVO buildMockRequestRecordVO(final MockRequestRecordDO mockRequestRecordDO) {
        return MockRequestRecordVO.builder()
                .id(mockRequestRecordDO.getId())
                .apiId(mockRequestRecordDO.getApiId())
                .header(mockRequestRecordDO.getHeader())
                .host(mockRequestRecordDO.getHost())
                .port(mockRequestRecordDO.getPort())
                .query(mockRequestRecordDO.getQuery())
                .pathVariable(mockRequestRecordDO.getPathVariable())
                .url(mockRequestRecordDO.getUrl())
                .body(mockRequestRecordDO.getBody())
                .dateCreated("")
                .dateUpdated("")
                .build();
    }

    public static final class MockRequestRecordVOBuilder {
        /**
         * primary key id.
         */
        private String id;

        /**
         * the api id.
         */
        private String apiId;

        /**
         * the request host.
         */
        private String host;

        /**
         * the request port.
         */
        private Integer port;

        /**
         * the request url.
         */
        private String url;

        /**
         * the request param in url.
         */
        private String pathVariable;

        /**
         * the request param after url.
         */
        private String query;

        /**
         * the request param in header.
         */
        private String header;

        /**
         * the request body.
         */
        private String body;

        /**
         * dateCreated.
         */
        private String dateCreated;

        /**
         * dateUpdated.
         */
        private String dateUpdated;

        /**
         * set id.
         * @param id id
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * set apiId.
         * @param apiId apiId
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder apiId(final String apiId) {
            this.apiId = apiId;
            return this;
        }

        /**
         * set host.
         * @param host host
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder host(final String host) {
            this.host = host;
            return this;
        }

        /**
         * set port.
         * @param port port
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder port(final Integer port) {
            this.port = port;
            return this;
        }

        /**
         * set url.
         * @param url url
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * set pathVariable.
         * @param pathVariable pathVariable
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder pathVariable(final String pathVariable) {
            this.pathVariable = pathVariable;
            return this;
        }

        /**
         * set query.
         * @param query query
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder query(final String query) {
            this.query = query;
            return this;
        }

        /**
         * set header.
         * @param header header
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder header(final String header) {
            this.header = header;
            return this;
        }

        /**
         * set body.
         * @param body body
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder body(final String body) {
            this.body = body;
            return this;
        }

        /**
         * set dateCreated.
         * @param dateCreated dateCreated
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder dateCreated(final String dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * set dateUpdated.
         * @param dateUpdated dateUpdated.
         * @return MockRequestRecordVOBuilder
         */
        public MockRequestRecordVOBuilder dateUpdated(final String dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         * @return MockRequestRecordVO
         */
        public MockRequestRecordVO build() {
            MockRequestRecordVO mockRequestRecordVO = new MockRequestRecordVO();
            mockRequestRecordVO.setId(id);
            mockRequestRecordVO.setApiId(apiId);
            mockRequestRecordVO.setDateCreated(dateCreated);
            mockRequestRecordVO.setDateUpdated(dateUpdated);
            mockRequestRecordVO.setHeader(header);
            mockRequestRecordVO.setHost(host);
            mockRequestRecordVO.setQuery(query);
            mockRequestRecordVO.setBody(body);
            mockRequestRecordVO.setUrl(url);
            mockRequestRecordVO.setPort(port);
            mockRequestRecordVO.setPathVariable(pathVariable);
            return mockRequestRecordVO;
        }
    }

}
