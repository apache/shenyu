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

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * this is scale rule view to web front.
 */
public class ScaleRuleVO implements Serializable {

    private static final long serialVersionUID = 5425558125437748538L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * metric name.
     */
    private String metricName;

    /**
     * type 0:shenyu 1:k8s 2:others.
     */
    private Integer type;

    /**
     * sort.
     */
    private Integer sort;

    /**
     * status 1:enable 0:disable.
     */
    private Integer status;

    /**
     * minimum of metric.
     */
    private String minimum;

    /**
     * maximum of metric.
     */
    private String maximum;

    /**
     * create time.
     */
    private String dateCreated;

    /**
     * update time.
     */
    private String dateUpdated;

    public ScaleRuleVO() {
    }

    public ScaleRuleVO(final String id,
                       final String metricName,
                       final Integer type,
                       final Integer sort,
                       final Integer status,
                       final String minimum,
                       final String maximum,
                       final String dateCreated,
                       final String dateUpdated) {
        this.id = id;
        this.metricName = metricName;
        this.type = type;
        this.sort = sort;
        this.status = status;
        this.minimum = minimum;
        this.maximum = maximum;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
     * Gets the value of sort.
     *
     * @return the value of sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * Sets the sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
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
     * Gets the value of minimum.
     *
     * @return the value of minimum
     */
    public String getMinimum() {
        return minimum;
    }

    /**
     * Sets the minimum.
     *
     * @param minimum minimum
     */
    public void setMinimum(final String minimum) {
        this.minimum = minimum;
    }

    /**
     * Gets the value of maximum.
     *
     * @return the value of maximum
     */
    public String getMaximum() {
        return maximum;
    }

    /**
     * Sets the maximum.
     *
     * @param maximum maximum
     */
    public void setMaximum(final String maximum) {
        this.maximum = maximum;
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
     * build scaleRuleVO.
     *
     * @param scaleRuleDO {@linkplain ScaleRuleDO}
     * @return {@linkplain ScaleRuleVO}
     */
    public static ScaleRuleVO buildScaleRuleVO(final ScaleRuleDO scaleRuleDO) {
        return Optional.ofNullable(scaleRuleDO)
                .map(item -> new ScaleRuleVO(item.getId(), item.getMetricName(), item.getType(),
                        item.getSort(), item.getStatus(), item.getMinimum(), item.getMaximum(),
                        DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ScaleRuleVO that = (ScaleRuleVO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(metricName, that.metricName)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(minimum, that.minimum)
                && Objects.equals(maximum, that.maximum)
                && Objects.equals(dateCreated, that.dateCreated)
                && Objects.equals(dateUpdated, that.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, metricName, type, sort, status, minimum, maximum, dateCreated, dateUpdated);
    }

    @Override
    public String toString() {
        return "ScaleRuleVO{"
                + "id='" + id + '\''
                + ", metricName='" + metricName + '\''
                + ", type=" + type
                + ", sort=" + sort
                + ", status=" + status
                + ", minimum='" + minimum + '\''
                + ", maximum='" + maximum + '\''
                + ", dateCreated='" + dateCreated + '\''
                + ", dateUpdated='" + dateUpdated + '\''
                + '}';
    }
}
