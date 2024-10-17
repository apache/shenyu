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
 * The scale rule query.
 */
public class ScaleRuleQuery implements Serializable {

    private static final long serialVersionUID = 8022510132462999321L;

    /**
     * metric name.
     */
    private String metricName;

    /**
     * type 0:shenyu 1:k8s 2:others.
     */
    private Integer type;

    /**
     * status 1:enable 0:disable.
     */
    private Integer status;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public ScaleRuleQuery() {
    }

    public ScaleRuleQuery(final String metricName, final Integer type, final Integer status, final PageParameter pageParameter) {
        this.metricName = metricName;
        this.pageParameter = pageParameter;
        this.type = type;
        this.status = status;
    }

    /**
     * Gets the value of metricName.
     *
     * @return the value of metricName
     */
    public String getMetricName() {
        return metricName;
    }

    /**
     * Sets the metricName.
     *
     * @param metricName metricName
     */
    public void setMetricName(final String metricName) {
        this.metricName = metricName;
    }

    /**
     * Gets the value of type.
     *
     * @return the value of type
     */
    public Integer getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type type
     */
    public void setType(final Integer type) {
        this.type = type;
    }

    /**
     * Gets the value of status.
     *
     * @return the value of status
     */
    public Integer getStatus() {
        return status;
    }

    /**
     * Sets the status.
     *
     * @param status status
     */
    public void setStatus(final Integer status) {
        this.status = status;
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
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ScaleRuleQuery that = (ScaleRuleQuery) o;
        return Objects.equals(metricName, that.metricName) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(metricName, pageParameter);
    }
}
