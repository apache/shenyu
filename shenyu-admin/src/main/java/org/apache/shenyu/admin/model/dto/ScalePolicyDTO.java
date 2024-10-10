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

import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.mapper.ScalePolicyMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * this is the scale policy from by web front.
 */
public class ScalePolicyDTO implements Serializable {

    private static final long serialVersionUID = 3319087894871820638L;


    /**
     * primary key id.
     */
    @Existed(provider = ScalePolicyMapper.class, nullOfIgnore = true, message = "scale policy is not existed")
    private String id;

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
     * number of bootstrap.
     */
    private Integer num;

    /**
     * begin time.
     */
    private LocalDateTime beginTime;

    /**
     * end time.
     */
    private LocalDateTime endTime;

    public ScalePolicyDTO() {
    }

    public ScalePolicyDTO(final String id,
                          final Integer sort,
                          final Integer status,
                          final Integer num,
                          final LocalDateTime beginTime,
                          final LocalDateTime endTime) {
        this.id = id;
        this.sort = sort;
        this.status = status;
        this.num = num;
        this.beginTime = beginTime;
        this.endTime = endTime;
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
     * Gets the value of num.
     *
     * @return the value of num
     */
    public Integer getNum() {
        return num;
    }

    /**
     * Sets the num.
     *
     * @param num num
     */
    public void setNum(final Integer num) {
        this.num = num;
    }

    /**
     * Gets the value of beginTime.
     *
     * @return the value of beginTime
     */
    public LocalDateTime getBeginTime() {
        return beginTime;
    }

    /**
     * Sets the beginTime.
     *
     * @param beginTime beginTime
     */
    public void setBeginTime(final LocalDateTime beginTime) {
        this.beginTime = beginTime;
    }

    /**
     * Gets the value of endTime.
     *
     * @return the value of endTime
     */
    public LocalDateTime getEndTime() {
        return endTime;
    }

    /**
     * Sets the endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final LocalDateTime endTime) {
        this.endTime = endTime;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static ScalePolicyDTO.ScalePolicyDTOBuilder builder() {
        return new ScalePolicyDTO.ScalePolicyDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ScalePolicyDTO that = (ScalePolicyDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(num, that.num)
                && Objects.equals(beginTime, that.beginTime)
                && Objects.equals(endTime, that.endTime);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, sort, status, num, beginTime, endTime);
    }

    public static final class ScalePolicyDTOBuilder {

        private String id;

        private Integer sort;

        private Integer status;

        private Integer num;

        private LocalDateTime beginTime;

        private LocalDateTime endTime;

        private ScalePolicyDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id primary key
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * sort.
         *
         * @param sort sort
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * status.
         *
         * @param status status
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder status(final Integer status) {
            this.status = status;
            return this;
        }

        /**
         * num.
         *
         * @param num num
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder num(final Integer num) {
            this.num = num;
            return this;
        }

        /**
         * beginTime.
         *
         * @param beginTime beginTime
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder beginTime(final LocalDateTime beginTime) {
            this.beginTime = beginTime;
            return this;
        }

        /**
         * endTime.
         *
         * @param endTime endTime
         * @return ScalePolicyDTOBuilder
         */
        public ScalePolicyDTOBuilder endTime(final LocalDateTime endTime) {
            this.endTime = endTime;
            return this;
        }

        /**
         * build.
         *
         * @return ScalePolicyDTO
         */
        public ScalePolicyDTO build() {
            ScalePolicyDTO scalePolicyDTO = new ScalePolicyDTO();
            scalePolicyDTO.setId(id);
            scalePolicyDTO.setSort(sort);
            scalePolicyDTO.setStatus(status);
            scalePolicyDTO.setNum(num);
            scalePolicyDTO.setBeginTime(beginTime);
            scalePolicyDTO.setEndTime(endTime);
            return scalePolicyDTO;
        }
    }
}
