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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

import com.google.gson.JsonObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocModule;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.service.manager.DocParser;
import org.apache.shenyu.common.utils.GsonUtils;
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

    /**
     * key:title, value:docInfo.
     */
    private static final Map<String, DocInfo> DOC_DEFINITION_MAP = new HashMap<>();

    /**
     * KEY:clusterName, value: md5.
     */
    private static final Map<String, String> CLUSTER_MD5_MAP = new HashMap<>();

    /**
     * key: DocItem.id, value: docInfo.
     */
    private static final Map<String, DocItem> ITEM_DOC_MAP = new ConcurrentHashMap<>(256);

    private static final DocParser SWAGGER_DOC_PARSER = new SwaggerDocParser();

    /**
     * add docInfo.
     *
     * @param clusterName clusterName
     * @param docInfoJson docInfoJson
     * @param callback    callback
     */
    @Override
    public void addDocInfo(final String clusterName, final String docInfoJson, final Consumer<DocInfo> callback) {
        if (StringUtils.isEmpty(docInfoJson)) {
            return;
        }
        String newMd5 = DigestUtils.md5DigestAsHex(docInfoJson.getBytes(StandardCharsets.UTF_8));
        String oldMd5 = CLUSTER_MD5_MAP.get(clusterName);
        if (Objects.equals(newMd5, oldMd5)) {
            return;
        }
        CLUSTER_MD5_MAP.put(clusterName, newMd5);
        DocInfo docInfo = getDocInfo(clusterName, docInfoJson);
        if (Objects.isNull(docInfo) || CollectionUtils.isEmpty(docInfo.getDocModuleList())) {
            return;
        }
        List<DocModule> docModules = docInfo.getDocModuleList();
        DOC_DEFINITION_MAP.put(docInfo.getTitle(), docInfo);
        docModules.forEach(docModule -> {
            docModule.getDocItems().forEach(docItem -> {
                ITEM_DOC_MAP.put(docItem.getId(), docItem);
            });
        });
        callback.accept(docInfo);
    }

    private DocInfo getDocInfo(final String clusterName, final String docInfoJson) {
        try {
            JsonObject docRoot = GsonUtils.getInstance().fromJson(docInfoJson, JsonObject.class);
            docRoot.addProperty("basePath", "/" + clusterName);
            DocInfo docInfo = SWAGGER_DOC_PARSER.parseJson(docRoot);
            docInfo.setClusterName(clusterName);
            return docInfo;
        } catch (Exception e) {
            LOG.error("getDocInfo error={}", e);
            return null;
        }
    }

    /**
     * get doc By Title.
     *
     * @param title title
     * @return DocInfo
     */
    @Override
    public DocInfo getByTitle(final String title) {
        return DOC_DEFINITION_MAP.get(title);
    }

    /**
     * getDocItem.
     *
     * @param id id
     * @return DocItem
     */
    @Override
    public DocItem getDocItem(final String id) {
        return ITEM_DOC_MAP.get(id);
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

    /**
     * getDocMd5.
     *
     * @param clusterName clusterName
     * @return String
     */
    @Override
    public String getDocMd5(final String clusterName) {
        return CLUSTER_MD5_MAP.get(clusterName);
    }

    /**
     * remove doc.
     *
     * @param clusterName clusterName
     */
    @Override
    public void remove(final String clusterName) {
        CLUSTER_MD5_MAP.remove(clusterName);
        DOC_DEFINITION_MAP.entrySet().removeIf(entry -> clusterName.equalsIgnoreCase(entry.getValue().getClusterName()));
    }
}
