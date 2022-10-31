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

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * The type Batch common dto.
 */
public class BatchCommonDTO implements Serializable {

    private static final long serialVersionUID = 7154784370528669046L;

    @NotEmpty
    @NotNull
    private List<@NotBlank String> ids;

    @NotNull
    private Boolean enabled;

    /**
     * Gets the value of ids.
     *
     * @return the value of ids
     */
    public List<String> getIds() {
        return ids;
    }

    /**
     * Sets the ids.
     *
     * @param ids ids
     */
    public void setIds(final List<String> ids) {
        this.ids = ids;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof BatchCommonDTO)) {
            return false;
        }
        BatchCommonDTO that = (BatchCommonDTO) o;
        return Objects.equals(ids, that.ids) && Objects.equals(enabled, that.enabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(ids, enabled);
    }
}
