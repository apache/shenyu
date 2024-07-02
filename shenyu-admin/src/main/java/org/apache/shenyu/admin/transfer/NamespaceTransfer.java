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

import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.vo.NamespaceVO;

import java.util.Optional;

public enum NamespaceTransfer {
    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * mapToVo.
     *
     * @param namespaceDO namespaceDO
     * @return NamespaceVO
     */
    public NamespaceVO mapToVo(NamespaceDO namespaceDO) {
        return Optional.ofNullable(namespaceDO).map(data -> {
            NamespaceVO vo = new NamespaceVO();
            vo.setId(data.getId());
            vo.setNamespaceId(data.getNamespaceId());
            vo.setName(data.getName());
            vo.setDescription(data.getDescription());
            return vo;
        }).orElse(null);
    }
}
