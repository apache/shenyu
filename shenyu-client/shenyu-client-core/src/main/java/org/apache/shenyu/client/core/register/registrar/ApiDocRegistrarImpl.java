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

package org.apache.shenyu.client.core.register.registrar;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.utils.ApiBeanUtil;
import org.apache.shenyu.client.core.utils.OpenApiUtils;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * ApiDocRegistrarImplImpl.
 *
 * @see ApiDocRegisterDTO
 */
public class ApiDocRegistrarImpl extends BaseApiRegistrarImpl {
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final ClientRegisterConfig clientRegisterConfig;
    
    public ApiDocRegistrarImpl(final ClientRegisterConfig clientRegisterConfig) {
        this.clientRegisterConfig = clientRegisterConfig;
    }
    
    @Override
    protected void doRegisterBean(final ApiBean apiBean) {
        // no supported
    }
    
    @Override
    protected void doRegisterApi(final ApiBean.ApiDefinition api) {
        publisher.publishEvent(ApiDocRegisterDTO.builder()
                .consume(getConsume(api))
                .produce(getProduce(api))
                .httpMethod(getHttpMethod(api))
                .contextPath(clientRegisterConfig.getContextPath())
                .document(getDocument(api))
                .rpcType(getRpcType(api))
                .version(getVersion(api))
                .apiDesc(getApiDesc(api))
                .ext(getExt(api))
                .tags(buildTags(api))
                .apiPath(getPath(api))
                .apiSource(getApiSource(api))
                .apiOwner(getOwner(api))
                .state(ApiStateEnum.PUBLISHED.getState())
                .eventType(EventType.REGISTER)
                .build());
    }
    
    private static Integer getHttpMethod(final ApiBean.ApiDefinition api) {
        final String httpMethod = api.getPropertiesValue("httpMethod");
        if (StringUtils.isNotBlank(httpMethod)) {
            return Integer.parseInt(httpMethod);
        }
        return ApiHttpMethodEnum.NOT_HTTP.getValue();
    }
    
    @NotNull
    private static String getOwner(final ApiBean.ApiDefinition api) {
        final String owner = api.getPropertiesValue("owner");
        if (StringUtils.isNotBlank(owner)) {
            return owner;
        }
        return "admin";
    }
    
    private String getExt(final ApiBean.ApiDefinition api) {
        final String ext = api.getPropertiesValue("ext");
        if (StringUtils.isNotBlank(ext)) {
            return ext;
        }
        ApiDocRegisterDTO.ApiExt apiExt = new ApiDocRegisterDTO.ApiExt();
        
        apiExt.setHost(clientRegisterConfig.getHost());
        apiExt.setPort(clientRegisterConfig.getPort());
        apiExt.setServiceName(api.getApiBean().getBeanClass().getName());
        apiExt.setMethodName(api.getApiMethodName());
        apiExt.setParameterTypes(api.getParameterTypes());
        apiExt.setRpcExt(api.getPropertiesValue("RpcExt"));
        apiExt.setAddPrefixed(clientRegisterConfig.getAddPrefixed());
        final String rpcType = getRpcType(api);
        
        if (Objects.equals(rpcType, RpcTypeEnum.HTTP.getName())) {
            apiExt.setProtocol(ShenyuClientConstants.HTTP);
        }
        if (Objects.equals(rpcType, RpcTypeEnum.WEB_SOCKET.getName())) {
            apiExt.setProtocol(ShenyuClientConstants.WS);
        }
        
        return GsonUtils.getInstance().toJson(apiExt);
    }
    
    private String getDocument(final ApiBean.ApiDefinition api) {
        final String document = api.getPropertiesValue("document");
        if (StringUtils.isNotBlank(document)) {
            return document;
        }
        final String path = getPath(api);
        final Map<String, Object> documentMap = ImmutableMap.<String, Object>builder()
                .put("tags", buildTags(api))
                .put("operationId", path)
                .put("parameters", OpenApiUtils.generateDocumentParameters(path, api.getApiMethod()))
                .put("responses", OpenApiUtils.generateDocumentResponse(path))
                .put("responseType", Collections.singletonList(OpenApiUtils.parseReturnType(api.getApiMethod())))
                .build();
        return GsonUtils.getInstance().toJson(documentMap);
    }
    
    private String getRpcType(final ApiBean.ApiDefinition api) {
        return ApiBeanUtil.getRpcType(api);
    }
    
    private String getVersion(final ApiBean.ApiDefinition api) {
        final String version = api.getPropertiesValue("version");
        if (StringUtils.isNotBlank(version)) {
            return version;
        }
        return "v0.01";
    }
    
    private String getApiDesc(final ApiBean.ApiDefinition api) {
        final String desc = api.getPropertiesValue("desc");
        if (StringUtils.isNotBlank(desc)) {
            return desc;
        }
        return api.getApiBean().getClientName()
                + ":"
                + api.getApiBean().getBeanClass().getName()
                + "#"
                + api.getApiMethod().getName();
    }
    
    private Integer getApiSource(final ApiBean.ApiDefinition apiDefinition) {
        final String apiSource = apiDefinition.getPropertiesValue("apiSource");
        if (StringUtils.isNotBlank(apiSource)) {
            return Integer.parseInt(apiSource);
        }
        return ApiSourceEnum.ANNOTATION_GENERATION.getValue();
    }
    
    private String getPath(final ApiBean.ApiDefinition api) {
        return PathUtils.pathJoin(clientRegisterConfig.getContextPath(), api.getApiBean().getBeanPath(), api.getMethodPath());
    }
    
    private String getProduce(final ApiBean.ApiDefinition api) {
        final String produce = api.getPropertiesValue("produce");
        if (StringUtils.isBlank(produce)) {
            return ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        }
        return produce;
    }
    
    private static String getConsume(final ApiBean.ApiDefinition api) {
        final String consume = api.getPropertiesValue("consume");
        if (StringUtils.isBlank(consume)) {
            return ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        }
        return consume;
    }
    
    private List<String> buildTags(final ApiBean.ApiDefinition api) {
        final String tags = api.getPropertiesValue("tags");
        if (StringUtils.isNotBlank(tags)) {
            return Arrays.stream(tags.split(","))
                    .collect(Collectors.toList());
        }
        return Collections.emptyList();
    }
}
