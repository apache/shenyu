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

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.io.Serializable;
import java.util.Objects;

/**
 * InstanceInfoVO.
 */
public class InstanceInfoVO implements Serializable {

    private static final long serialVersionUID = -5798225577222473005L;

    /**
     * id.
     */
    private String id;
    
    /**
     * instance ip.
     */
    private String instanceIp;
    
    /**
     * instance type.
     */
    private String instanceType;
    
    /**
     * instance info.
     */
    private String instanceInfo;
    
    /**
     * namespaceId.
     */
    @NotBlank
    @Existed(message = "namespaceId is not existed", provider = NamespaceMapper.class)
    private String namespaceId;


    /**
     * get namespaceId.
     *
     * @return namespaceId
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * set namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }

        InstanceInfoVO that = (InstanceInfoVO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(instanceIp, that.instanceIp)
                && Objects.equals(instanceType, that.instanceType)
                && Objects.equals(instanceInfo, that.instanceInfo)
                && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, instanceIp, instanceType, instanceInfo, namespaceId);
    }
}
