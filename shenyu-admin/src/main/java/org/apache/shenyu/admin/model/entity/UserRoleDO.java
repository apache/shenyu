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
import org.apache.shenyu.admin.model.dto.UserRoleDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * The User Role Entity.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class UserRoleDO extends BaseDO {

    private static final long serialVersionUID = -6072114067735588550L;

    /**
     * user key.
     */
    private String userId;

    /**
     * role key.
     */
    private String roleId;

    /**
     * build UserRoleDO.
     *
     * @param userRoleDTO {@linkplain UserRoleDTO}
     * @return {@linkplain UserRoleDO}
     */
    public static UserRoleDO buildUserRoleDO(final UserRoleDTO userRoleDTO) {
        return Optional.ofNullable(userRoleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            UserRoleDO userRoleDO = UserRoleDO.builder()
                    .roleId(item.getRoleId())
                    .userId(item.getUserId())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                userRoleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                userRoleDO.setDateCreated(currentTime);
            } else {
                userRoleDO.setId(item.getId());
            }
            return userRoleDO;
        }).orElse(null);
    }
}
