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

package org.apache.shenyu.admin.service.manager.impl;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

import com.google.gson.JsonObject;
import jakarta.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocModule;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.service.manager.DocParser;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.DigestUtils;

/**
 * Doc Manager.
 */
@Service
public class DocManagerImpl implements DocManager {
    private static final Logger LOG = LoggerFactory.getLogger(DocManagerImpl.class);

    private static final String API_DOC_VERSION = "v0.01";

    /**
     * The constant HTTP.
     */
    private static final String HTTP = "http://";

    /**
     * key:title, value:docInfo.
     */
    private static final Map<String, DocInfo> DOC_DEFINITION_MAP = new HashMap<>();

    private static final DocParser SWAGGER_DOC_PARSER = new SwaggerDocParser();

    @Resource
    private RegisterApiDocService registerApiDocService;

    /**
     * add docInfo.
     *
     * @param instance    instance
     * @param docInfoJson docInfoJson
     * @param callback    callback
     */
    @Override
    public void addDocInfo(final UpstreamInstance instance, final String docInfoJson, final String oldMd5, final Consumer<DocInfo> callback) {
        if (StringUtils.isEmpty(docInfoJson)) {
            return;
        }
        String newMd5 = DigestUtils.md5DigestAsHex(docInfoJson.getBytes(StandardCharsets.UTF_8));
        if (Objects.equals(newMd5, oldMd5)) {
            return;
        }
        DocInfo docInfo = getDocInfo(instance.getClusterName(), docInfoJson);
        if (Objects.isNull(docInfo) || CollectionUtils.isEmpty(docInfo.getDocModuleList())) {
            return;
        }
        docInfo.setDocMd5(newMd5);

        List<DocModule> docModules = docInfo.getDocModuleList();
        docModules.forEach(docModule -> docModule.getDocItems().forEach(docItem -> {
            ApiDocRegisterDTO build = ApiDocRegisterDTO.builder()
                .consume(this.getProduceConsume(docItem.getConsumes()))
                .produce(this.getProduceConsume(docItem.getProduces()))
                .httpMethod(this.getHttpMethod(docItem))
                .contextPath(docInfo.getContextPath())
                .ext(this.buildExtJson(instance, docItem))
                .document(JsonUtils.toJson(docItem))
                .rpcType(RpcTypeEnum.HTTP.getName())
                .version(API_DOC_VERSION)
                .apiDesc(docItem.getDescription())
                .tags(Collections.singletonList(docInfo.getContextPath()))
                .apiPath(docItem.getName())
                .apiSource(ApiSourceEnum.SWAGGER.getValue())
                .state(ApiStateEnum.UNPUBLISHED.getState())
                .apiOwner("admin")
                .eventType(EventType.REGISTER)
                .build();

            registerApiDocService.registerApiDocument(build);
        }));

        callback.accept(docInfo);
    }

    private String getProduceConsume(final Collection<String> list) {
        String res = StringUtils.EMPTY;
        if (Objects.nonNull(list)) {
            Optional<String> first = list.stream().findFirst();
            if (first.isPresent()) {
                res = first.get();
            }
        }
        return StringUtils.isNotEmpty(res) ? res : "*/*";
    }

    private Integer getHttpMethod(final DocItem docItem) {
        Integer httpMethod = null;
        Optional<String> first = docItem.getHttpMethodList().stream().findFirst();
        if (first.isPresent()) {
            String method = docItem.getHttpMethodList().size() == 1 ? StringUtils.upperCase(first.get()) : ApiHttpMethodEnum.GET.getName();
            httpMethod = ApiHttpMethodEnum.getValueByName(method);
        }
        return httpMethod;
    }

    private DocInfo getDocInfo(final String clusterName, final String docInfoJson) {
        try {
            JsonObject docRoot = GsonUtils.getInstance().fromJson(docInfoJson, JsonObject.class);
            String contexPath = "/" + clusterName;
            docRoot.addProperty("basePath", contexPath);
            DocInfo docInfo = SWAGGER_DOC_PARSER.parseJson(docRoot);
            docInfo.setClusterName(clusterName);
            docInfo.setContextPath(contexPath);
            return docInfo;
        } catch (Exception e) {
            LOG.error("getDocInfo clusterName={} error", clusterName, e);
            return null;
        }
    }

    private String buildExtJson(final UpstreamInstance instance, final DocItem docItem) {
        ApiDocRegisterDTO.ApiExt ext = new ApiDocRegisterDTO.ApiExt();
        ext.setHost(instance.getIp());
        ext.setPort(instance.getPort());
        ext.setServiceName(instance.getClusterName());
        ext.setMethodName(docItem.getName());
        ext.setParameterTypes("");
        ext.setRpcExt(null);
        ext.setAddPrefixed(false);
        ext.setProtocol(HTTP);
        return GsonUtils.getInstance().toJson(ext);
    }

    /**
     * get DocInfo.
     *
     * @return Collection
     */
    @Override
    public Collection<DocInfo> listAll() {
        return DOC_DEFINITION_MAP.values();
    }
}
