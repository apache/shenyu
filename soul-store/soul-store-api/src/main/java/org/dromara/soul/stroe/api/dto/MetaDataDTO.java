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

package org.dromara.soul.stroe.api.dto;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

/**
 * The type Meta data dto.
 * @author xiaoyu
 */
@Data
@EqualsAndHashCode
public class MetaDataDTO implements Serializable {

    private String id;

    /**
     * The application name.
     */
    private String appName;

    /**
     * the request api gateway path.
     */
    private String path;

    /**
     * the rpcType {@linkplain org.dromara.soul.common.enums.RpcTypeEnum}
     */
    private String rpcType;

    /**
     * The Full path package name of the service.
     */
    private String serviceName;

    /**
     * The methodName.
     */
    private String methodName;

    /**
     * The parameter Types,Full path package name of the service,please use ','separation.
     */
    private String parameterTypes;

    /**
     * The rpc ext param, please save json data.
     */
    private String rpcExt;

    /**
     * whether enabled.
     */
    private Boolean enabled;


}
