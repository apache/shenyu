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
 * The type Auth path do.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class AuthPathDO extends BaseDO {

    private static final long serialVersionUID = 2265360597468199607L;

    private String authId;

    private String appName;

    private String path;

    private Boolean enabled;

    /**
     * Build AuthPathDO object with given params.
     *
     * @param path      {@linkplain String}
     * @param authId    {@linkplain String}
     * @param appName   {@linkplain String}
     * @return          {@linkplain AuthPathDO}
     */
    public static AuthPathDO create(final String path, final String authId, final String appName) {
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        return AuthPathDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .authId(authId)
                .appName(appName)
                .path(path)
                .enabled(true)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
    }
}
