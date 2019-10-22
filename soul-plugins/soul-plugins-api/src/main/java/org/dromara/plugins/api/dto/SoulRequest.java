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

package org.dromara.plugins.api.dto;

import lombok.Data;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.http.HttpMethod;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * @author xiaoyu(Myth)
 */
@Data
public class SoulRequest implements Serializable {

    /**
     * is rpcType data. now we only support "http","dubbo" "springCloud".
     * {@linkplain RpcTypeEnum}
     */
    private String rpcType;

    private HttpMethod httpMethod;

    /**
     * The Headers.
     */
    private Map<String, String> headers;

    /**
     * The parameters. http headers, parameters.
     */
    private Map<String, String> parameters;

    /**
     * startDateTime
     */
    private LocalDateTime startDateTime;

    private String path;

    /**
     * http body ，is json...
     */
    private String body;

    private String httpPath;

    private String url;

    /**
     * accessKey .
     */
    private String accessKey;

    /**
     * this is sign .
     */
    private String sign;

    /**
     * timestamp .
     */
    private String timestamp;
}
