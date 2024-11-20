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

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.Objects;

/**
 * this is the scale rule from by web front.
 */
public class ScaleRuleDTO implements Serializable {

    private static final long serialVersionUID = 7696616914460256129L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * metric name.
     */
    @NotBlank
    private String metricName;

    /**
     * type 0:shenyu 1:k8s 2:others.
     */
    @NotNull
    private Integer type;

    /**
     * sort.
     */
    @NotNull
    private Integer sort;

    /**
     * status 1:enable 0:disable.
     */
    @NotNull
    private Integer status;

    /**
     * minimum of metric.
     */
    private String minimum;

    /**
     * maximum of metric.
     */
    private String maximum;

    public ScaleRuleDTO() {
    }

    public ScaleRuleDTO(final String id,
                        @NotBlank final String metricName,
                        @NotNull final Integer type,
                        @NotNull final Integer sort,
                        @NotNull final Integer status,
                        final String minimum,
                        final String maximum) {
        this.id = id;
        this.metricName = metricName;
        this.type = type;
        this.sort = sort;
        this.status = status;
        this.minimum = minimum;
        this.maximum = maximum;
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
     * builder.
     *
     * @return ScaleRuleDTOBuilder
     */
    public static ScaleRuleDTO.ScaleRuleDTOBuilder builder() {
        return new ScaleRuleDTO.ScaleRuleDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ScaleRuleDTO that = (ScaleRuleDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(metricName, that.metricName)
                && Objects.equals(type, that.type)
                && Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(minimum, that.minimum)
                && Objects.equals(maximum, that.maximum);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, metricName, type, sort, status, minimum, maximum);
    }

    public static final class ScaleRuleDTOBuilder {

        private String id;

        private String metricName;

        private Integer type;

        private Integer sort;

        private Integer status;

        private String minimum;

        private String maximum;

        private ScaleRuleDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id id
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * metricName.
         *
         * @param metricName metricName
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder metricName(final String metricName) {
            this.metricName = metricName;
            return this;
        }

        /**
         * type.
         *
         * @param type type
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * sort.
         *
         * @param sort sort
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * status.
         *
         * @param status status
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder status(final Integer status) {
            this.status = status;
            return this;
        }

        /**
         * minimum.
         *
         * @param minimum minimum
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder minimum(final String minimum) {
            this.minimum = minimum;
            return this;
        }

        /**
         * maximum.
         *
         * @param maximum maximum
         * @return ScaleRuleDTOBuilder
         */
        public ScaleRuleDTOBuilder maximum(final String maximum) {
            this.maximum = maximum;
            return this;
        }

        /**
         * build.
         *
         * @return ScaleRuleDTO
         */
        public ScaleRuleDTO build() {
            ScaleRuleDTO scaleRuleDTO = new ScaleRuleDTO();
            scaleRuleDTO.setId(id);
            scaleRuleDTO.setMetricName(metricName);
            scaleRuleDTO.setType(type);
            scaleRuleDTO.setSort(sort);
            scaleRuleDTO.setStatus(status);
            scaleRuleDTO.setMinimum(minimum);
            scaleRuleDTO.setMaximum(maximum);
            return scaleRuleDTO;
        }
    }
}
