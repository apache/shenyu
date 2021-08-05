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

import java.util.Objects;

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
        if (Objects.isNull(appAuthDTO)) {
            return null;
        }

        AppAuthDO.AppAuthDOBuilder<?, ?> appAuthDO = AppAuthDO.builder();
        appAuthDO.id(appAuthDTO.getId());
        appAuthDO.appKey(appAuthDTO.getAppKey());
        appAuthDO.appSecret(appAuthDTO.getAppSecret());
        appAuthDO.enabled(appAuthDTO.getEnabled());
        appAuthDO.open(appAuthDTO.getOpen());
        appAuthDO.userId(appAuthDTO.getUserId());
        appAuthDO.phone(appAuthDTO.getPhone());
        appAuthDO.extInfo(appAuthDTO.getExtInfo());

        return appAuthDO.build();
    }

    /**
     * Map to vo app auth vo.
     *
     * @param appAuthDO the app auth do
     * @return the app auth vo
     */
    public AppAuthVO mapToVO(final AppAuthDO appAuthDO) {
        if (Objects.isNull(appAuthDO)) {
            return null;
        }

        AppAuthVO appAuthVO = new AppAuthVO();
        appAuthVO.setId(appAuthDO.getId());
        appAuthVO.setAppKey(appAuthDO.getAppKey());
        appAuthVO.setAppSecret(appAuthDO.getAppSecret());
        appAuthVO.setUserId(appAuthDO.getUserId());
        appAuthVO.setPhone(appAuthDO.getPhone());
        appAuthVO.setExtInfo(appAuthDO.getExtInfo());
        appAuthVO.setOpen(appAuthDO.getOpen());
        appAuthVO.setEnabled(appAuthDO.getEnabled());
        if (Objects.nonNull(appAuthDO.getDateUpdated())) {
            appAuthVO.setDateUpdated(DateUtils.localDateTimeToString(appAuthDO.getDateUpdated().toLocalDateTime()));
        }

        return appAuthVO;
    }

}
