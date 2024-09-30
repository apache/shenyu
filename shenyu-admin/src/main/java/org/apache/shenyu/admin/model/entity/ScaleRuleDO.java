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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ScaleRuleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * Table: scale_rule.
 */
public final class ScaleRuleDO extends BaseDO {

    private static final long serialVersionUID = 8778323510074951149L;

    /**
     * Column: metric_name.
     * Type: VARCHAR(128).
     * Remark: metric name.
     */
    private String metricName;

    /**
     * Column: type.
     * Type: INT.
     * Remark: type 0:shenyu 1:k8s 2:others.
     */
    private Integer type;

    /**
     * Column: sort.
     * Type: INT.
     * Remark: sort.
     */
    private Integer sort;

    /**
     * Column: status.
     * Type: INT.
     * Remark: status 1:enable 0:disable.
     */
    private Integer status;

    /**
     * Column: minimum.
     * Type: VARCHAR(128).
     * Remark: minimum of metric.
     */
    private String minimum;

    /**
     * Column: maximum.
     * Type: VARCHAR(128).
     * Remark: maximum of metric.
     */
    private String maximum;

    public ScaleRuleDO() {
    }

    public ScaleRuleDO(final String metricName, final Integer type, final Integer sort, final Integer status, final String minimum, final String maximum) {
        this.metricName = metricName;
        this.type = type;
        this.sort = sort;
        this.status = status;
        this.minimum = minimum;
        this.maximum = maximum;
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
     * builder.
     *
     * @return ScaleRuleDOBuilder
     */
    public static ScaleRuleDO.ScaleRuleDOBuilder builder() {
        return new ScaleRuleDO.ScaleRuleDOBuilder();
    }

    /**
     * build ScaleRuleDO.
     *
     * @param scaleRuleDTO scaleRuleDTO
     * @return ScaleRuleDO
     */
    public static ScaleRuleDO buildScaleRuleDO(final ScaleRuleDTO scaleRuleDTO) {
        ScaleRuleDO scaleRuleDO = new ScaleRuleDO();
        if (StringUtils.isEmpty(scaleRuleDTO.getId())) {
            scaleRuleDO.setId(UUIDUtils.getInstance().generateShortUuid());
            scaleRuleDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        } else {
            scaleRuleDO.setId(scaleRuleDTO.getId());
        }
        return Optional.of(scaleRuleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            scaleRuleDO.setMetricName(item.getMetricName());
            scaleRuleDO.setType(item.getType());
            scaleRuleDO.setSort(item.getSort());
            scaleRuleDO.setStatus(item.getStatus());
            scaleRuleDO.setMinimum(item.getMinimum());
            scaleRuleDO.setMaximum(item.getMaximum());
            scaleRuleDO.setDateUpdated(currentTime);
            return scaleRuleDO;
        }).orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        ScaleRuleDO that = (ScaleRuleDO) o;
        return Objects.equals(metricName, that.metricName)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(minimum, that.minimum)
                && Objects.equals(maximum, that.maximum);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), metricName, type, sort, status, minimum, maximum);
    }

    public static final class ScaleRuleDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String metricName;

        private Integer type;

        private Integer sort;

        private Integer status;

        private String minimum;

        private String maximum;

        private ScaleRuleDOBuilder() {
        }

        /**
         * metricName.
         *
         * @param metricName metricName
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder metricName(final String metricName) {
            this.metricName = metricName;
            return this;
        }

        /**
         * type.
         *
         * @param type type
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * sort.
         *
         * @param sort sort
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * status.
         *
         * @param status status
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder status(final Integer status) {
            this.status = status;
            return this;
        }

        /**
         * minimum.
         *
         * @param minimum minimum
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder minimum(final String minimum) {
            this.minimum = minimum;
            return this;
        }

        /**
         * maximum.
         *
         * @param maximum maximum
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder maximum(final String maximum) {
            this.maximum = maximum;
            return this;
        }

        /**
         * id.
         *
         * @param id id
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return ScaleRuleDOBuilder
         */
        public ScaleRuleDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return ScaleRuleDO
         */
        public ScaleRuleDO build() {
            ScaleRuleDO scaleRuleDO = new ScaleRuleDO();
            scaleRuleDO.setId(id);
            scaleRuleDO.setDateCreated(dateCreated);
            scaleRuleDO.setDateUpdated(dateUpdated);
            scaleRuleDO.setMetricName(metricName);
            scaleRuleDO.setType(type);
            scaleRuleDO.setSort(sort);
            scaleRuleDO.setStatus(status);
            scaleRuleDO.setMinimum(minimum);
            scaleRuleDO.setMaximum(maximum);
            return scaleRuleDO;
        }
    }
}
