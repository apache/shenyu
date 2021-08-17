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

import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

/**
 * data permission page list vo.
 */
public class DataPermissionPageVO implements Serializable {

    private static final long serialVersionUID = -7532270386821533624L;

    /**
     * selector id or rule id.
     */
    private String dataId;

    /**
     * selector name or rule name.
     */
    private String dataName;

    /**
     * whether checked.
     */
    private Boolean isChecked;

    public DataPermissionPageVO() {
    }

    public DataPermissionPageVO(final String dataId, final String dataName, final Boolean isChecked) {
        this.dataId = dataId;
        this.dataName = dataName;
        this.isChecked = isChecked;
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
     * Gets the value of dataName.
     *
     * @return the value of dataName
     */
    public String getDataName() {
        return dataName;
    }

    /**
     * Sets the dataName.
     *
     * @param dataName dataName
     */
    public void setDataName(final String dataName) {
        this.dataName = dataName;
    }

    /**
     * Gets the value of isChecked.
     *
     * @return the value of isChecked
     */
    public Boolean getIsChecked() {
        return isChecked;
    }

    /**
     * Sets the isChecked.
     *
     * @param isChecked isChecked
     */
    public void setIsChecked(final Boolean isChecked) {
        this.isChecked = isChecked;
    }

    /**
     * build vo by selector.
     * @param selectorDO {@linkplain SelectorDO}
     * @param isChecked whether checked
     * @return {@linkplain DataPermissionPageVO}
     */
    public static DataPermissionPageVO buildPageVOBySelector(final SelectorDO selectorDO, final Boolean isChecked) {
        return Optional.ofNullable(selectorDO)
                .map(item -> new DataPermissionPageVO(item.getId(), item.getName(), isChecked))
                .orElse(null);
    }

    /**
     * build data permission page vo by rule do.
     * @param ruleDO {@linkplain RuleDO}
     * @param isChecked whether checked
     * @return  {@linkplain DataPermissionPageVO}
     */
    public static DataPermissionPageVO buildPageVOByRule(final RuleDO ruleDO, final Boolean isChecked) {
        return Optional.of(ruleDO)
                .map(item -> new DataPermissionPageVO(item.getId(), item.getName(), isChecked))
                .orElse(null);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof DataPermissionPageVO)) {
            return false;
        }
        DataPermissionPageVO that = (DataPermissionPageVO) o;
        return Objects.equals(dataId, that.dataId) && Objects.equals(dataName, that.dataName) && Objects.equals(isChecked, that.isChecked);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dataId, dataName, isChecked);
    }

    @Override
    public String toString() {
        return "DataPermissionPageVO{"
                + "dataId='" + dataId + '\''
                + ", dataName='" + dataName + '\''
                + ", isChecked=" + isChecked
                + '}';
    }
}
