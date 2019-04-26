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
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.HttpMethodEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.util.MultiValueMap;

import java.io.Serializable;

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
     * this is dubbo params.
     */
    private String dubboParams;

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
     * content is json data.
     */
    private String content;

    /**
     * extInfo is json data .
     */
    private String extInfo;

    /**
     * pathVariable
     */
    private String pathVariable;

    /**
     * ServerHttpRequest transform RequestDTO .
     *
     * @param request {@linkplain ServerHttpRequest}
     * @return RequestDTO request dto
     */
    public static RequestDTO transform(final ServerHttpRequest request) {
        final String module = request.getHeaders().getFirst(Constants.MODULE);
        final String method = request.getHeaders().getFirst(Constants.METHOD);
        final String appKey = request.getHeaders().getFirst(Constants.APP_KEY);
        final String httpMethod = request.getHeaders().getFirst(Constants.HTTP_METHOD);
        final String rpcType = request.getHeaders().getFirst(Constants.RPC_TYPE);
        final String sign = request.getHeaders().getFirst(Constants.SIGN);
        final String timestamp = request.getHeaders().getFirst(Constants.TIMESTAMP);
        final String extInfo = request.getHeaders().getFirst(Constants.EXT_INFO);
        final String pathVariable = request.getHeaders().getFirst(Constants.PATH_VARIABLE);
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setModule(module);
        requestDTO.setMethod(method);
        requestDTO.setAppKey(appKey);
        requestDTO.setHttpMethod(httpMethod);
        requestDTO.setRpcType(rpcType);
        requestDTO.setSign(sign);
        requestDTO.setTimestamp(timestamp);
        requestDTO.setExtInfo(extInfo);
        requestDTO.setPathVariable(pathVariable);
        return requestDTO;
    }

    public static RequestDTO transformMap(MultiValueMap<String, String> queryParams) {
        RequestDTO requestDTO = new RequestDTO();
        requestDTO.setModule(queryParams.getFirst(Constants.MODULE));
        requestDTO.setMethod(queryParams.getFirst(Constants.METHOD));
        requestDTO.setRpcType(queryParams.getFirst(Constants.RPC_TYPE));
        return requestDTO;
    }

}
