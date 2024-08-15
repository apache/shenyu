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

import org.apache.shenyu.admin.model.page.PageParameter;

import java.io.Serializable;
import java.util.Objects;

/**
 * this is plugin query.
 */
public class ApiQuery implements Serializable {

    private static final long serialVersionUID = 167659024501717438L;

    /**
     * apiPath.
     */
    private String apiPath;


    /**
     * state.
     */
    private Integer state;


    /**
     * tagId.
     */
    private String tagId;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public ApiQuery() {
    }

    public ApiQuery(final String apiPath, final Integer state, final String tagId, final PageParameter pageParameter) {
        this.apiPath = apiPath;
        this.state = state;
        this.tagId = tagId;
        this.pageParameter = pageParameter;
    }

    /**
     * get apiPath.
     *
     * @return apiPath
     */
    public String getApiPath() {
        return apiPath;
    }

    /**
     * set apiPath.
     *
     * @param apiPath apiPath
     */
    public void setApiPath(final String apiPath) {
        this.apiPath = apiPath;
    }

    /**
     * get state.
     *
     * @return state
     */
    public Integer getState() {
        return state;
    }

    /**
     * set state.
     *
     * @param state state
     */
    public void setState(final Integer state) {
        this.state = state;
    }

    /**
     * get tagId.
     *
     * @return tagId
     */
    public String getTagId() {
        return tagId;
    }

    /**
     * set tagId.
     *
     * @param tagId tagId
     */
    public void setTagId(final String tagId) {
        this.tagId = tagId;
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
        if (!(o instanceof ApiQuery)) {
            return false;
        }
        ApiQuery apiQuery = (ApiQuery) o;
        return apiPath.equals(apiQuery.apiPath) && state.equals(apiQuery.state) && tagId.equals(apiQuery.tagId) && pageParameter.equals(apiQuery.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(apiPath, state, tagId, pageParameter);
    }
}
