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
 * this is resource query.
 */
public class ResourceQuery implements Serializable {

    private static final long serialVersionUID = 3454409443837608172L;

    /**
     * resource title.
     */
    private String title;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public ResourceQuery() {
    }

    public ResourceQuery(final String title, final PageParameter pageParameter) {
        this.title = title;
        this.pageParameter = pageParameter;
    }

    /**
     * Gets the value of title.
     *
     * @return the value of title
     */
    public String getTitle() {
        return title;
    }

    /**
     * Sets the title.
     *
     * @param title title
     */
    public void setTitle(final String title) {
        this.title = title;
    }

    /**
     * Gets the value of pageParameter.
     *
     * @return the value of pageParameter
     */
    public PageParameter getPageParameter() {
        return pageParameter;
    }

    /**
     * Sets the pageParameter.
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
        if (!(o instanceof ResourceQuery)) {
            return false;
        }
        ResourceQuery that = (ResourceQuery) o;
        return Objects.equals(title, that.title) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(title, pageParameter);
    }
}
