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
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;
import java.util.Optional;

/**
 * Table: scale_policy.
 */
public final class ScalePolicyDO extends BaseDO {

    private static final long serialVersionUID = -6895279885108899135L;

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
     * Column: num.
     * Type: INT.
     * Remark: number of bootstrap.
     */
    private Integer num;

    /**
     * Column: begin_time.
     * Type: DATETIME.
     * Remark: begin time.
     */
    private Date beginTime;

    /**
     * Column: end_time.
     * Type: DATETIME.
     * Remark: end time.
     */
    private Date endTime;

    /**
     * Column: date_created.
     * Type: TIMESTAMP.
     * Default value: CURRENT_TIMESTAMP(3).
     * Remark: create time.
     */
    private Date dateCreated;

    /**
     * Column: date_updated.
     * Type: TIMESTAMP.
     * Default value: CURRENT_TIMESTAMP(3).
     * Remark: update time.
     */
    private Date dateUpdated;

    public ScalePolicyDO() {
    }

    public ScalePolicyDO(final Integer sort, final Integer status, final Integer num, final Date beginTime, final Date endTime) {
        this.sort = sort;
        this.status = status;
        this.num = num;
        this.beginTime = beginTime;
        this.endTime = endTime;
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
    public Date getBeginTime() {
        return beginTime;
    }

    /**
     * Sets the beginTime.
     *
     * @param beginTime beginTime
     */
    public void setBeginTime(final Date beginTime) {
        this.beginTime = beginTime;
    }

    /**
     * Gets the value of endTime.
     *
     * @return the value of endTime
     */
    public Date getEndTime() {
        return endTime;
    }

    /**
     * Sets the endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final Date endTime) {
        this.endTime = endTime;
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
        ScalePolicyDO that = (ScalePolicyDO) o;
        return Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(num, that.num)
                && Objects.equals(beginTime, that.beginTime)
                && Objects.equals(endTime, that.endTime);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), sort, status, num, beginTime, endTime);
    }

    /**
     * builder.
     *
     * @return ScalePolicyDOBuilder
     */
    public static ScalePolicyDO.ScalePolicyDOBuilder builder() {
        return new ScalePolicyDO.ScalePolicyDOBuilder();
    }

    /**
     * buildScalePolicyDO.
     *
     * @param scalePolicyDTO scalePolicyDTO
     * @return ScalePolicyDO
     */
    public static ScalePolicyDO buildScalePolicyDO(final ScalePolicyDTO scalePolicyDTO) {
        return Optional.ofNullable(scalePolicyDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            try {
                ScalePolicyDO scalePolicyDO =
                        ScalePolicyDO.builder()
                                .sort(item.getSort())
                                .status(item.getStatus())
                                .num(item.getNum())
                                .beginTime(item.getBeginTime() != null && DateUtils.isValidDate(DateUtils.localDateTimeToString(item.getBeginTime()))
                                        ? new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(DateUtils.localDateTimeToString(item.getBeginTime())) : null)
                                .endTime(item.getEndTime() != null && DateUtils.isValidDate(DateUtils.localDateTimeToString(item.getEndTime()))
                                        ? new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(DateUtils.localDateTimeToString(item.getEndTime())) : null)
                                .dateUpdated(currentTime)
                                .build();
                if (StringUtils.isEmpty(item.getId())) {
                    scalePolicyDO.setId(UUIDUtils.getInstance().generateShortUuid());
                    scalePolicyDO.setDateCreated(currentTime);
                } else {
                    scalePolicyDO.setId(item.getId());
                }
                return scalePolicyDO;
            } catch (ParseException e) {
                throw new RuntimeException(e);
            }
        }).orElse(null);
    }

    public static final class ScalePolicyDOBuilder {

        private String id;

        private Integer sort;

        private Integer status;

        private Integer num;

        private Date beginTime;

        private Date endTime;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private ScalePolicyDOBuilder() {
        }

        /**
         * id.
         *
         * @param id id
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * sort.
         *
         * @param sort sort
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder sort(final Integer sort) {
            this.sort = sort;
            return this;
        }

        /**
         * status.
         *
         * @param status status
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder status(final Integer status) {
            this.status = status;
            return this;
        }

        /**
         * num.
         *
         * @param num num
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder num(final Integer num) {
            this.num = num;
            return this;
        }

        /**
         * beginTime.
         *
         * @param beginTime beginTime
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder beginTime(final Date beginTime) {
            this.beginTime = beginTime;
            return this;
        }

        /**
         * endTime.
         *
         * @param endTime endTime
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder endTime(final Date endTime) {
            this.endTime = endTime;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return ScalePolicyDOBuilder
         */
        public ScalePolicyDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return ScalePolicyDO
         */
        public ScalePolicyDO build() {
            ScalePolicyDO scalePolicyDO = new ScalePolicyDO();
            scalePolicyDO.setId(id);
            scalePolicyDO.setSort(sort);
            scalePolicyDO.setStatus(status);
            scalePolicyDO.setNum(num);
            scalePolicyDO.setBeginTime(beginTime);
            scalePolicyDO.setEndTime(endTime);
            scalePolicyDO.setDateCreated(dateCreated);
            scalePolicyDO.setDateUpdated(dateUpdated);
            return scalePolicyDO;
        }
    }
}
