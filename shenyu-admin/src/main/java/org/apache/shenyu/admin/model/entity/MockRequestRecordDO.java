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

package org.apache.shenyu.admin.model.entity;

import java.sql.Timestamp;

/**
 * The Mock Request Record Entity.
 */
public class MockRequestRecordDO extends BaseDO {

    private static final long serialVersionUID = -30960666013060928L;

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
     * builder.
     *
     * @return {@linkplain MockRequestRecordDOBuilder}
     */
    public static MockRequestRecordDOBuilder builder() {
        return new MockRequestRecordDOBuilder();
    }

    public static final class MockRequestRecordDOBuilder {

        private String id;
        
        private String apiId;
        
        private String host;
        
        private Integer port;

        private String url;
        
        private String pathVariable;
        
        private String query;
        
        private String header;
        
        private String body;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        public MockRequestRecordDOBuilder() {
        }
        
        /**
         * id.
         *
         * @param id the id.
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
        
        /**
         * Sets the apiId.
         *
         * @param apiId apiId
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder apiId(final String apiId) {
            this.apiId = apiId;
            return this;
        }

        /**
         * Sets the host.
         *
         * @param host host
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder host(final String host) {
            this.host = host;
            return this;
        }

        /**
         * Sets the port.
         *
         * @param port port
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder port(final Integer port) {
            this.port = port;
            return this;
        }

        /**
         * Sets the url.
         *
         * @param url url
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder url(final String url) {
            this.url = url;
            return this;
        }

        /**
         * Sets the pathVariable.
         *
         * @param pathVariable pathVariable
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder pathVariable(final String pathVariable) {
            this.pathVariable = pathVariable;
            return this;
        }

        /**
         * Sets the query.
         *
         * @param query query
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder query(final String query) {
            this.query = query;
            return this;
        }

        /**
         * Sets the header.
         *
         * @param header header
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder header(final String header) {
            this.header = header;
            return this;
        }

        /**
         * Sets the body.
         *
         * @param body body
         * @return MockRequestRecordDOBuilder.
         */
        public MockRequestRecordDOBuilder body(final String body) {
            this.body = body;
            return this;
        }

        /**
         * build MockRequestRecordDO.
         *
         * @return {@linkplain MockRequestRecordDO}
         */
        public MockRequestRecordDO build() {
            MockRequestRecordDO mockRequestRecordDO = new MockRequestRecordDO();
            mockRequestRecordDO.setId(id);
            mockRequestRecordDO.setApiId(apiId);
            mockRequestRecordDO.setBody(body);
            mockRequestRecordDO.setHeader(header);
            mockRequestRecordDO.setHost(host);
            mockRequestRecordDO.setPort(port);
            mockRequestRecordDO.setUrl(url);
            mockRequestRecordDO.setPathVariable(pathVariable);
            mockRequestRecordDO.setQuery(query);
            mockRequestRecordDO.setDateCreated(dateCreated);
            mockRequestRecordDO.setDateUpdated(dateUpdated);
            return mockRequestRecordDO;
        }
    }

}

