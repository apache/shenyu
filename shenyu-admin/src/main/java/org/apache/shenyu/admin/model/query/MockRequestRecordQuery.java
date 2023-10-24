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

package org.apache.shenyu.admin.model.query;

import java.io.Serializable;
import java.util.Objects;
import org.apache.shenyu.admin.model.page.PageParameter;

/**
 * this is mock request record query.
 */
public class MockRequestRecordQuery implements Serializable {

    private static final long serialVersionUID = 6736947701814601503L;

    /**
     * the api id.
     */
    private String apiId;

    /**
     * the request host.
     */
    private String host;

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
    private String header;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public MockRequestRecordQuery() {

    }

    public MockRequestRecordQuery(final String apiId, final String host, final String url,
                                  final String pathVariable, final String header, final PageParameter pageParameter) {
        this.apiId = apiId;
        this.host = host;
        this.url = url;
        this.pageParameter = pageParameter;
        this.pathVariable = pathVariable;
        this.header = header;
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
     * get pageParameter.
     *
     * @return pageParameter
     */
    public PageParameter getPageParameter() {
        return pageParameter;
    }

    /**
     * set pageParameter.
     *
     * @param pageParameter pageParameter
     */
    public void setPageParameter(final PageParameter pageParameter) {
        this.pageParameter = pageParameter;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MockRequestRecordQuery)) {
            return false;
        }
        MockRequestRecordQuery mockRequestRecordQuery = (MockRequestRecordQuery) o;
        return apiId.equals(mockRequestRecordQuery.apiId) && header.equals(mockRequestRecordQuery.getHeader())
                && host.equals(mockRequestRecordQuery.getHost()) && url.equals(mockRequestRecordQuery.getUrl())
                && pathVariable.equals(mockRequestRecordQuery.getPathVariable());
    }

    @Override
    public int hashCode() {
        return Objects.hash(apiId, header, host, url, pathVariable);
    }
}
