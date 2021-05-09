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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

/**
 * DashboardUserDO.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class DashboardUserDO extends BaseDO {

    private static final long serialVersionUID = 3464935043890680423L;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * current role list.
     */
    private List<String> roles;

    /**
     * build dashboardUserDO.
     *
     * @param dashboardUserDTO {@linkplain DashboardUserDTO}
     * @return {@linkplain DashboardUserDO}
     */
    public static DashboardUserDO buildDashboardUserDO(final DashboardUserDTO dashboardUserDTO) {
        return Optional.ofNullable(dashboardUserDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            DashboardUserDO dashboardUserDO = DashboardUserDO.builder()
                    .userName(item.getUserName())
                    .password(item.getPassword())
                    .role(item.getRole())
                    .roles(item.getRoles())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                dashboardUserDO.setId(UUIDUtils.getInstance().generateShortUuid());
                dashboardUserDO.setEnabled(true);
                dashboardUserDO.setDateCreated(currentTime);
            } else {
                dashboardUserDO.setId(item.getId());
                dashboardUserDO.setEnabled(item.getEnabled());
            }
            return dashboardUserDO;
        }).orElse(null);
    }
}
