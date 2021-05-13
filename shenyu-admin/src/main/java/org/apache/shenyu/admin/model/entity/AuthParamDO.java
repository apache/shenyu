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
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;

/**
 * The type Auth param do.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class AuthParamDO extends BaseDO {

    private static final long serialVersionUID = -6719996683886817375L;

    private String authId;

    private String appName;

    private String appParam;

    /**
     * Build AuthParamDO object with given params.
     *
     * @param authId    {@linkplain String}
     * @param appName   {@linkplain String}
     * @param appParam  {@linkplain String}
     * @return          {@linkplain AuthParamDO}
     */
    public static AuthParamDO create(final String authId, final String appName, final String appParam) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return AuthParamDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .authId(authId)
                .appName(appName)
                .appParam(appParam)
                .dateUpdated(currentTime)
                .dateCreated(currentTime)
                .build();
    }
}
