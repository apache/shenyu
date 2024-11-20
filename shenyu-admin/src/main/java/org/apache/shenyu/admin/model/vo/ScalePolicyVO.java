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

import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * this is scale policy view to web front.
 */
public class ScalePolicyVO implements Serializable {

    private static final long serialVersionUID = 7948061709049446961L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * sort.
     */
    private Integer sort;

    /**
     * status 1:enable 0:disable.
     */
    private Integer status;

    /**
     * number of bootstrap.
     */
    private Integer num;

    /**
     * begin time.
     */
    private String beginTime;

    /**
     * end time.
     */
    private String endTime;

    /**
     * create time.
     */
    private String dateCreated;

    /**
     * update time.
     */
    private String dateUpdated;

    public ScalePolicyVO() {
    }

    public ScalePolicyVO(final String id,
                         final Integer sort,
                         final Integer status,
                         final Integer num,
                         final String beginTime,
                         final String endTime,
                         final String dateCreated,
                         final String dateUpdated) {
        this.id = id;
        this.sort = sort;
        this.status = status;
        this.num = num;
        this.beginTime = beginTime;
        this.endTime = endTime;
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
    public String getBeginTime() {
        return beginTime;
    }

    /**
     * Sets the beginTime.
     *
     * @param beginTime beginTime
     */
    public void setBeginTime(final String beginTime) {
        this.beginTime = beginTime;
    }

    /**
     * Gets the value of endTime.
     *
     * @return the value of endTime
     */
    public String getEndTime() {
        return endTime;
    }

    /**
     * Sets the endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final String endTime) {
        this.endTime = endTime;
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
     * buildScalePolicyVO.
     *
     * @param scalePolicyDO scalePolicyDO
     * @return ScalePolicyVO
     */
    public static ScalePolicyVO buildScalePolicyVO(final ScalePolicyDO scalePolicyDO) {
        return Optional.ofNullable(scalePolicyDO)
                .map(item -> {
                    String beginTime = item.getBeginTime() != null
                            ? DateUtils.localDateTimeToString(new Timestamp(item.getBeginTime().getTime()).toLocalDateTime()) : null;
                    String endTime = item.getEndTime() != null
                            ? DateUtils.localDateTimeToString(new Timestamp(item.getEndTime().getTime()).toLocalDateTime()) : null;
                    return new ScalePolicyVO(item.getId(), item.getSort(), item.getStatus(), item.getNum(),
                            beginTime,
                            endTime,
                            DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                            DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()));
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
        ScalePolicyVO that = (ScalePolicyVO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(sort, that.sort)
                && Objects.equals(status, that.status)
                && Objects.equals(num, that.num)
                && Objects.equals(beginTime, that.beginTime)
                && Objects.equals(endTime, that.endTime)
                && Objects.equals(dateCreated, that.dateCreated)
                && Objects.equals(dateUpdated, that.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, sort, status, num, beginTime, endTime, dateCreated, dateUpdated);
    }

    @Override
    public String toString() {
        return "ScalePolicyVO{"
                + "id='" + id + '\''
                + ", sort=" + sort
                + ", status=" + status
                + ", num=" + num
                + ", beginTime='" + beginTime + '\''
                + ", endTime='" + endTime + '\''
                + ", dateCreated='" + dateCreated + '\''
                + ", dateUpdated='" + dateUpdated + '\''
                + '}';
    }
}
