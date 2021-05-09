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
import org.apache.shenyu.admin.model.entity.RoleDO;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is role view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RoleVO implements Serializable {

    private static final long serialVersionUID = 2783609252111382305L;

    /**
     * primary key.
     */
    private String id;

    /**
     * role name.
     */
    private String roleName;

    /**
     * description.
     */
    private String description;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build roleVO.
     *
     * @param roleDO {@linkplain RoleDO}
     * @return {@linkplain RoleVO}
     */
    public static RoleVO buildRoleVO(final RoleDO roleDO) {
        return Optional.ofNullable(roleDO)
                .map(item -> new RoleVO(item.getId(), item.getRoleName(),
                        item.getDescription(), DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                        DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }
}
