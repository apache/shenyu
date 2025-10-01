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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.entity.RegistryDO;
import org.apache.shenyu.admin.model.vo.RegistryVO;

import java.util.Optional;

public enum RegistryTransfer {
    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * mapToVo.
     *
     * @param registryDO registryDO
     * @return RegistryVO
     */
    public RegistryVO mapToVo(RegistryDO registryDO) {
        return Optional.ofNullable(registryDO).map(data -> {
            RegistryVO vo = new RegistryVO();
            vo.setId(data.getId());
            vo.setRegistryId(data.getRegistryId());
            vo.setProtocol(data.getProtocol());
            vo.setAddress(data.getAddress());
            vo.setUsername(data.getUsername());
            vo.setPassword(data.getPassword());
            vo.setGroup(data.getRegistryGroup());
            vo.setNamespace(data.getNamespace());
            return vo;
        }).orElse(null);
    }
}
