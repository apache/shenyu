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

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * The type Auth path warp dto.
 */
public class AuthPathWarpDTO implements Serializable {

    private static final long serialVersionUID = -3167442906221294444L;
    
    @Existed(message = "app key not existed", provider = AppAuthMapper.class)
    private String id;

    @NotEmpty(message = "auth path is not empty")
    private List<@Valid AuthPathDTO> authPathDTOList;

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
     * Gets the value of authPathDTOList.
     *
     * @return the value of authPathDTOList
     */
    public List<AuthPathDTO> getAuthPathDTOList() {
        return authPathDTOList;
    }

    /**
     * Sets the authPathDTOList.
     *
     * @param authPathDTOList authPathDTOList
     */
    public void setAuthPathDTOList(final List<AuthPathDTO> authPathDTOList) {
        this.authPathDTOList = authPathDTOList;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AuthPathWarpDTO)) {
            return false;
        }
        AuthPathWarpDTO that = (AuthPathWarpDTO) o;
        return Objects.equals(id, that.id) && Objects.equals(authPathDTOList, that.authPathDTOList);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, authPathDTOList);
    }
}
