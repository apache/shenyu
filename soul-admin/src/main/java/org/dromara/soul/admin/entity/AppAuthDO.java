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

package org.dromara.soul.admin.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.dto.AuthApplyDTO;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * AppAuthDO.
 *
 * @author xiaoyu(Myth)
 * @author nuo-promise
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class AppAuthDO extends BaseDO {

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    private String userId;

    private String phone;

    private String extInfo;

    /**
     * Build AppAuthDO object with given AppAuthDTO object.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO create(final AppAuthDTO appAuthDTO) {
        return Optional.ofNullable(appAuthDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            AppAuthDO appAuthDO = AppAuthDO.builder()
                    .appKey(item.getAppKey())
                    .appSecret(item.getAppSecret())
                    .enabled(item.getEnabled())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                appAuthDO.setId(UUIDUtils.getInstance().generateShortUuid());
                appAuthDO.setDateCreated(currentTime);
            } else {
                appAuthDO.setId(item.getId());
            }
            return appAuthDO;
        }).orElse(null);
    }

    /**
     * Build AppAuthDO object with given AuthApplyDTO object.
     *
     * @param authApplyDTO {@linkplain AuthApplyDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO create(final AuthApplyDTO authApplyDTO) {
        return Optional.ofNullable(authApplyDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            return AppAuthDO.builder()
                    .id(UUIDUtils.getInstance().generateShortUuid())
                    .userId(item.getUserId())
                    .phone(item.getPhone())
                    .extInfo(item.getExtInfo())
                    .appKey(SignUtils.getInstance().generateKey())
                    .appSecret(SignUtils.getInstance().generateKey())
                    .enabled(true)
                    .dateCreated(currentTime)
                    .dateUpdated(currentTime)
                    .build();
        }).orElse(null);
    }
}
