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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Time;
import java.sql.Timestamp;
import java.util.Optional;

/**
 * AppAuthDO.
 *
 * @author xiaoyu(Myth)
 * @author nuo-promise
 */
@Data
@Builder
@EqualsAndHashCode(callSuper = true)
public class AppAuthDO extends BaseDO {

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

    @Builder(toBuilder = true)
    public AppAuthDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated, final String appKey,
                     final String appSecret, final Boolean enabled, final String userId, final String phone, final String extInfo) {
        super(id, dateCreated, dateUpdated);
        this.appKey = appKey;
        this.appSecret = appSecret;
        this.enabled = enabled;
        this.userId = userId;
        this.phone = phone;
        this.extInfo = extInfo;
    }

    /**
     * build appAuthDO.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO buildAppAuthDO(final AppAuthDTO appAuthDTO) {
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
}
