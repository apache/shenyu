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

import java.sql.Timestamp;
import java.util.Date;
import java.util.Objects;

/**
 * ScaleHistoryDO.
 */
public final class ScaleHistoryDO extends BaseDO {

    private static final long serialVersionUID = 9073404091200662252L;

    /**
     * primary key.
     */
    private String id;

    /**
     * config id.
     */
    private Integer configId;

    /**
     * number of bootstrap.
     */
    private Integer num;

    /**
     * action.
     */
    private Integer action;

    /**
     * message.
     */
    private String msg;

    /**
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

    public ScaleHistoryDO() {
    }

    public ScaleHistoryDO(final Integer configId, final Integer num, final Integer action, final String msg) {
        this.configId = configId;
        this.num = num;
        this.action = action;
        this.msg = msg;
    }

    /**
     * Gets the value of configId.
     *
     * @return the value of configId
     */
    public Integer getConfigId() {
        return configId;
    }

    /**
     * Sets the configId.
     *
     * @param configId configId
     */
    public void setConfigId(final Integer configId) {
        this.configId = configId;
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
     * Gets the value of action.
     *
     * @return the value of action
     */
    public Integer getAction() {
        return action;
    }

    /**
     * Sets the action.
     *
     * @param action action
     */
    public void setAction(final Integer action) {
        this.action = action;
    }

    /**
     * Gets the value of msg.
     *
     * @return the value of msg
     */
    public String getMsg() {
        return msg;
    }

    /**
     * Sets the msg.
     *
     * @param msg msg
     */
    public void setMsg(final String msg) {
        this.msg = msg;
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
        ScaleHistoryDO that = (ScaleHistoryDO) o;
        return Objects.equals(configId, that.configId) && Objects.equals(num, that.num) && Objects.equals(action, that.action) && Objects.equals(msg, that.msg);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), configId, num, action, msg);
    }

    public static final class ScaleHistoryDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Integer configId;

        private Integer num;

        private Integer action;

        private String msg;

        private Timestamp dateUpdated;

        private ScaleHistoryDOBuilder() {
        }

        /**
         * id.
         *
         * @param id id
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * configId.
         *
         * @param configId configId
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder configId(final Integer configId) {
            this.configId = configId;
            return this;
        }

        /**
         * num.
         *
         * @param num num
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder num(final Integer num) {
            this.num = num;
            return this;
        }

        /**
         * action.
         *
         * @param action action
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder action(final Integer action) {
            this.action = action;
            return this;
        }

        /**
         * msg.
         *
         * @param msg msg
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder msg(final String msg) {
            this.msg = msg;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return ScaleHistoryDOBuilder
         */
        public ScaleHistoryDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return ScaleHistoryDO
         */
        public ScaleHistoryDO build() {
            ScaleHistoryDO scaleHistoryDO = new ScaleHistoryDO(configId, num, action, msg);
            scaleHistoryDO.setId(id);
            scaleHistoryDO.setDateCreated(dateCreated);
            scaleHistoryDO.setDateUpdated(dateUpdated);
            return scaleHistoryDO;
        }
    }
}
