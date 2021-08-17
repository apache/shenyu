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

import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.common.utils.DateUtils;

import java.util.Optional;

/**
 * The interface App auth transfer.
 */
public enum AppAuthTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to entity app auth do.
     *
     * @param appAuthDTO the app auth dto
     * @return the app auth do
     */
    public AppAuthDO mapToEntity(final AppAuthDTO appAuthDTO) {
        return Optional.ofNullable(appAuthDTO)
                .map(v -> {
                    AppAuthDO.AppAuthDOBuilder appAuthDO = AppAuthDO.builder();
                    appAuthDO.id(v.getId());
                    appAuthDO.appKey(v.getAppKey());
                    appAuthDO.appSecret(v.getAppSecret());
                    appAuthDO.enabled(v.getEnabled());
                    appAuthDO.open(v.getOpen());
                    appAuthDO.userId(v.getUserId());
                    appAuthDO.phone(v.getPhone());
                    appAuthDO.extInfo(v.getExtInfo());
                    return appAuthDO.build();
                })
                .orElse(null);
    }

    /**
     * Map to vo app auth vo.
     *
     * @param appAuthDO the app auth do
     * @return the app auth vo
     */
    public AppAuthVO mapToVO(final AppAuthDO appAuthDO) {
        return Optional.ofNullable(appAuthDO)
                .map(v -> {
                    AppAuthVO appAuthVO = new AppAuthVO();
                    appAuthVO.setId(v.getId());
                    appAuthVO.setAppKey(v.getAppKey());
                    appAuthVO.setAppSecret(v.getAppSecret());
                    appAuthVO.setUserId(v.getUserId());
                    appAuthVO.setPhone(v.getPhone());
                    appAuthVO.setExtInfo(v.getExtInfo());
                    appAuthVO.setOpen(v.getOpen());
                    appAuthVO.setEnabled(appAuthDO.getEnabled());
                    appAuthVO.setDateUpdated(Optional.ofNullable(appAuthDO.getDateUpdated())
                            .map(u -> DateUtils.localDateTimeToString(u.toLocalDateTime()))
                            .orElse(null));
                    return appAuthVO;
                })
                .orElse(null);
    }

}
