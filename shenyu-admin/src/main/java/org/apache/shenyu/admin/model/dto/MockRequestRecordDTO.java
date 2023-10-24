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

import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.Date;
import java.util.Objects;

/**
 * this is mockrequestrecord from by web front.
 */
public class MockRequestRecordDTO implements Serializable {

    private static final long serialVersionUID = -6779456713216687114L;

    /**
     * primary key.
     */
    private String id;

    /**
     * apiId.
     */
    @Existed(provider = ApiMapper.class, nullOfIgnore = true, message = "the apiId is not exited")
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
     * whole url,such as curl http://domain//test1/**?param=test .
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
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

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
     * getDateCreated.
     *
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MockRequestRecordDTO)) {
            return false;
        }
        MockRequestRecordDTO dto = (MockRequestRecordDTO) o;
        return Objects.equals(id, dto.id) && Objects.equals(apiId, dto.getApiId())
                && Objects.equals(host, dto.getHost()) && Objects.equals(pathVariable, dto.getPathVariable())
                && Objects.equals(query, dto.getQuery()) && Objects.equals(header, dto.getHeader())
                && Objects.equals(body, dto.getBody()) && Objects.equals(dateCreated, dto.dateCreated)
                && Objects.equals(dateUpdated, dto.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, apiId, host, pathVariable, query, header, body, dateCreated, dateUpdated);
    }

}
