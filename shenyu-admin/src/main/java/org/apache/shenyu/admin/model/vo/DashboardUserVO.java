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

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is dashboard user view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DashboardUserVO implements Serializable {

    private static final long serialVersionUID = -6493573459758664389L;

    /**
     * primary key.
     */
    private String id;

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
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build dashboardUserVO.
     *
     * @param dashboardUserDO {@linkplain DashboardUserDO}
     * @return {@linkplain DashboardUserVO}
     */
    public static DashboardUserVO buildDashboardUserVO(final DashboardUserDO dashboardUserDO) {
        return Optional.ofNullable(dashboardUserDO)
                .map(item -> new DashboardUserVO(item.getId(), item.getUserName(),
                        item.getPassword(), item.getRole(), item.getEnabled(),
                        DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime())))
                .orElse(null);
    }
}
