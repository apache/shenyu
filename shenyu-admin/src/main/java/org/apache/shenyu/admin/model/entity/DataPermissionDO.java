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

import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * The Data Permission Entity.
 */
public final class DataPermissionDO extends BaseDO {

    private static final long serialVersionUID = 8732493731708038311L;

    /**
     * user id.
     */
    private String userId;

    /**
     * selector or  rule id.
     */
    private String dataId;

    /**
     * selector or rule type： 0: Selector, 1: Rule.
     */
    private Integer dataType;

    public DataPermissionDO() {
    }

    public DataPermissionDO(final String userId, final String dataId, final Integer dataType) {
        this.userId = userId;
        this.dataId = dataId;
        this.dataType = dataType;
    }

    /**
     * Gets the value of userId.
     *
     * @return the value of userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Sets the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    /**
     * Gets the value of dataId.
     *
     * @return the value of dataId
     */
    public String getDataId() {
        return dataId;
    }

    /**
     * Sets the dataId.
     *
     * @param dataId dataId
     */
    public void setDataId(final String dataId) {
        this.dataId = dataId;
    }

    /**
     * Gets the value of dataType.
     *
     * @return the value of dataType
     */
    public Integer getDataType() {
        return dataType;
    }

    /**
     * Sets the dataType.
     *
     * @param dataType dataType
     */
    public void setDataType(final Integer dataType) {
        this.dataType = dataType;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static DataPermissionDO.DataPermissionDOBuilder builder() {
        return new DataPermissionDO.DataPermissionDOBuilder();
    }

    /**
     * build Permission DO.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return {@linkplain DataPermissionDO}
     */
    public static DataPermissionDO buildPermissionDO(final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            DataPermissionDO dataPermissionDo = DataPermissionDO.builder()
                    .userId(item.getUserId())
                    .dataId(item.getDataId())
                    .dataType(item.getDataType())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                dataPermissionDo.setId(UUIDUtils.getInstance().generateShortUuid());
                dataPermissionDo.setDateCreated(currentTime);
            } else {
                dataPermissionDo.setId(item.getId());
            }
            return dataPermissionDo;
        }).orElse(null);
    }

    /**
     * build permission do by RuleDO and user id.
     * @param dataId rule id
     * @param userId user id
     * @param dataType data type
     * @return {@linkplain DataPermissionDO}
     */
    public static DataPermissionDO buildCreatePermissionDO(final String dataId, final String userId, final Integer dataType) {

        Timestamp currentTime = new Timestamp(System.currentTimeMillis());

        return DataPermissionDO.builder()
                .userId(userId)
                .dataId(dataId)
                .dataType(dataType)
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(currentTime)
                .build();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DataPermissionDO)) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        DataPermissionDO that = (DataPermissionDO) o;
        return Objects.equals(userId, that.userId) && Objects.equals(dataId, that.dataId) && Objects.equals(dataType, that.dataType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), userId, dataId, dataType);
    }

    public static final class DataPermissionDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String userId;

        private String dataId;

        private Integer dataType;

        private DataPermissionDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * id.
         *
         * @param dateCreated the dateCreated.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * userId.
         *
         * @param userId the userId.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder userId(final String userId) {
            this.userId = userId;
            return this;
        }

        /**
         * dataId.
         *
         * @param dataId the dataId.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder dataId(final String dataId) {
            this.dataId = dataId;
            return this;
        }

        /**
         * dataType.
         *
         * @param dataType the dataType.
         * @return DataPermissionDOBuilder.
         */
        public DataPermissionDOBuilder dataType(final Integer dataType) {
            this.dataType = dataType;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public DataPermissionDO build() {
            DataPermissionDO dataPermissionDO = new DataPermissionDO();
            dataPermissionDO.setId(id);
            dataPermissionDO.setDateCreated(dateCreated);
            dataPermissionDO.setDateUpdated(dateUpdated);
            dataPermissionDO.setUserId(userId);
            dataPermissionDO.setDataId(dataId);
            dataPermissionDO.setDataType(dataType);
            return dataPermissionDO;
        }
    }
}
