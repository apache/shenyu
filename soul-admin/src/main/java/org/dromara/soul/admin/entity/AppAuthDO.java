/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.admin.entity;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;

/**
 * AppAuthDO.
 *
 * @author xiaoyu(Myth)
 */
@Data
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

    /**
     * build appAuthDO.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return {@linkplain AppAuthDO}
     */
    public static AppAuthDO buildAppAuthDO(final AppAuthDTO appAuthDTO) {
        if (appAuthDTO != null) {
            AppAuthDO appAuthDO = new AppAuthDO();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            if (StringUtils.isEmpty(appAuthDTO.getId())) {
                appAuthDO.setId(UUIDUtils.getInstance().generateShortUuid());
                appAuthDO.setDateCreated(currentTime);
            } else {
                appAuthDO.setId(appAuthDTO.getId());
            }

            appAuthDO.setAppKey(appAuthDTO.getAppKey());
            appAuthDO.setAppSecret(appAuthDTO.getAppSecret());
            appAuthDO.setEnabled(appAuthDTO.getEnabled());
            appAuthDO.setDateUpdated(currentTime);
            return appAuthDO;
        }
        return null;
    }
}
