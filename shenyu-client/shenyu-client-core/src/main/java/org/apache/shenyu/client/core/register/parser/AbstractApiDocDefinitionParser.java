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

package org.apache.shenyu.client.core.register.parser;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.utils.OpenApiUtils;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public abstract class AbstractApiDocDefinitionParser implements ApiDocDefinitionParser {

    private static final String API_DOC_VERSION = "v0.01";

    private final RpcTypeEnum rpcTypeEnum;

    private final String host;

    private final Integer port;

    private final Boolean addPrefixed;

    public AbstractApiDocDefinitionParser(final ClientRegisterConfig clientRegisterConfig) {
        this.rpcTypeEnum = clientRegisterConfig.getRpcTypeEnum();
        this.host = clientRegisterConfig.getHost();
        this.port = clientRegisterConfig.getPort();
        this.addPrefixed = clientRegisterConfig.getAddPrefixed();
    }

    @Override
    public List<ApiDocRegisterDTO> parse(final ApiBean.ApiDefinition apiDefinition) {

        ApiDoc apiDoc = apiDefinition.getAnnotation(ApiDoc.class);

        List<String> tags = apiDoc.tags().length == 1 && StringUtils.isBlank(apiDoc.tags()[0]) ? Lists.newArrayList() : Arrays.asList(apiDoc.tags());

        String desc = apiDoc.desc();

        String apiPath = apiDefinition.getApiPath();

        HttpApiSpecificInfo httpApiSpecificInfo = doParse(apiDefinition);

        List<ApiDocRegisterDTO> apiDocRegisters = new ArrayList<>();

        String documentJson = buildDocumentJson(tags, apiPath, apiDefinition.getApiMethod());

        String extJson = buildExtJson(apiDefinition);

        for (ApiHttpMethodEnum apiHttpMethodEnum : httpApiSpecificInfo.apiHttpMethodEnums) {
            ApiDocRegisterDTO build = ApiDocRegisterDTO.builder()
                    .consume(httpApiSpecificInfo.consume)
                    .produce(httpApiSpecificInfo.produce)
                    .httpMethod(apiHttpMethodEnum.getValue())
                    .contextPath(apiDefinition.getContextPath())
                    .ext(extJson)
                    .document(documentJson)
                    .rpcType(rpcTypeEnum.getName())
                    .version(API_DOC_VERSION)
                    .apiDesc(desc)
                    .tags(tags)
                    .apiPath(apiPath)
                    .apiSource(ApiSourceEnum.ANNOTATION_GENERATION.getValue())
                    .state(ApiStateEnum.PUBLISHED.getState())
                    .apiOwner("admin")
                    .eventType(EventType.REGISTER)
                    .build();
            apiDocRegisters.add(build);
        }
        return apiDocRegisters;
    }

    private String buildDocumentJson(final List<String> tags, final String path, final Method method) {
        Map<String, Object> documentMap = ImmutableMap.<String, Object>builder()
                .put("tags", tags)
                .put("operationId", path)
                .put("parameters", OpenApiUtils.generateDocumentParameters(path, method))
                .put("responses", OpenApiUtils.generateDocumentResponse(path)).build();
        return GsonUtils.getInstance().toJson(documentMap);
    }

    private String buildExtJson(final ApiBean.ApiDefinition apiDefinition) {

        ApiDocRegisterDTO.ApiExt ext = new ApiDocRegisterDTO.ApiExt();

        ext.setHost(host);
        ext.setPort(port);
        ext.setServiceName(apiDefinition.getServiceName());
        ext.setMethodName(apiDefinition.getApiMethodName());
        ext.setParameterTypes(apiDefinition.getParameterTypes());
        ext.setRpcExt(apiDefinition.getRpcExt());
        ext.setAddPrefixed(addPrefixed);

        if (rpcTypeEnum == RpcTypeEnum.HTTP) {
            ext.setProtocol(ShenyuClientConstants.HTTP);
        }

        if (rpcTypeEnum == RpcTypeEnum.WEB_SOCKET) {
            ext.setProtocol(ShenyuClientConstants.WS);
        }

        return GsonUtils.getInstance().toJson(ext);
    }

    protected abstract HttpApiSpecificInfo doParse(ApiBean.ApiDefinition apiDefinition);

    public static class HttpApiSpecificInfo {

        private final String produce;

        private final String consume;

        private final List<ApiHttpMethodEnum> apiHttpMethodEnums;

        public HttpApiSpecificInfo(final String produce,
                                   final String consume,
                                   final List<ApiHttpMethodEnum> apiHttpMethodEnums) {
            this.produce = produce;
            this.consume = consume;
            this.apiHttpMethodEnums = apiHttpMethodEnums;
        }
    }
}
