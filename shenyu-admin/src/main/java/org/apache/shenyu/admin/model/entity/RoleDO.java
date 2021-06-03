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

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * The Role Data Entity.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class RoleDO extends BaseDO {

    private static final long serialVersionUID = -7319631396664845158L;

    /**
     * role name.
     */
    private String roleName;

    /**
     * description.
     */
    private String description;

    /**
     * build RoleDO.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return {@linkplain RoleDO}
     */
    public static RoleDO buildRoleDO(final RoleDTO roleDTO) {
        return Optional.ofNullable(roleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            RoleDO roleDO = RoleDO.builder()
                    .roleName(item.getRoleName())
                    .description(item.getDescription())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                roleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                roleDO.setDateCreated(currentTime);
            } else {
                roleDO.setId(item.getId());
            }
            return roleDO;
        }).orElse(null);
    }
}
