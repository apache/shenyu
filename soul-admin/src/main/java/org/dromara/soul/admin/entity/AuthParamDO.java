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

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.sql.Timestamp;

/**
 * The type Auth param do.
 *
 * @author xiaoyu
 * @author nuo-promise
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class AuthParamDO extends BaseDO {

    private String authId;

    private String appName;

    private String appParam;

    @Builder
    private AuthParamDO(final String id, final Timestamp dateCreated, final Timestamp dateUpdated,
                       final String authId, final String appName, final String appParam) {
        super(id, dateCreated, dateUpdated);
        this.authId = authId;
        this.appName = appName;
        this.appParam = appParam;
    }
}
