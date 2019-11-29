/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.request;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.MultiValueMap;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * the soul request DTO .
 *
 * @author xiaoyu(Myth)
 */
@Data
public class RequestDTO implements Serializable {

    /**
     * is module data.
     */
    private String module;

    /**
     * is method name .
     */
    private String method;

    /**
     * is rpcType data. now we only support "http","dubbo" "springCloud".
     * {@linkplain RpcTypeEnum}
     */
    private String rpcType;

    /**
     * httpMethod now we only support "get","post" .
     * {@linkplain  HttpMethodEnum}
     */
    private String httpMethod;

    /**
     * this is sign .
     */
    private String sign;

    /**
     * timestamp .
     */
    private String timestamp;

    /**
     * appKey .
     */
    private String appKey;

    /**
     * path.
     */
    private String path;


    /**
     * the contextPath.
     */
    private String contextPath;

    /**
     * realUrl.
     */
    private String realUrl;

    /**
     * the metaData.
     */
    private MetaData metaData;

    /**
     * this is dubbo params.
     */
    private String dubboParams;

    /**
     * startDateTime.
     */
    private LocalDateTime startDateTime;

    /**
     * Transform map request dto.
     *
     * @param queryParams the query params
     * @return the request dto
     */
    public static RequestDTO transformMap(final MultiValueMap<String, String> queryParams) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setModule(queryParams.getFirst(Constants.MODULE));
        requestDTO.setMethod(queryParams.getFirst(Constants.METHOD));
        requestDTO.setRpcType(queryParams.getFirst(Constants.RPC_TYPE));
        return requestDTO;
    }

}
