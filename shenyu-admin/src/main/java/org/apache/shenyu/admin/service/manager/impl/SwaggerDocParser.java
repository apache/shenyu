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

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.shenyu.admin.model.bean.CustomCode;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocModule;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.admin.service.manager.DocParser;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

/**
 * Parse the JSON content of swagger.
 */
public class SwaggerDocParser implements DocParser {

    /**
     * parseJson.
     *
     * @param docRoot docRoot
     * @return DocInfo
     */
    @Override
    public DocInfo parseJson(final JSONObject docRoot) {
        final String basePath = docRoot.getString("basePath");
        final String title = Optional.ofNullable(docRoot.getJSONObject("info")).map(jsonObject -> jsonObject.getString("title")).orElse(basePath);
        final List<DocItem> docItems = new ArrayList<>();

        JSONObject paths = docRoot.getJSONObject("paths");
        if (paths == null) {
            paths = new JSONObject();
        }
        Set<String> pathNameSet = paths.keySet();
        for (String apiPath : pathNameSet) {
            JSONObject pathInfo = paths.getJSONObject(apiPath);
            // key: get,post,head...
            Collection<String> httpMethodList = getHttpMethods(pathInfo);
            Optional<String> first = httpMethodList.stream().findFirst();
            if (first.isPresent()) {
                String method = first.get();
                JSONObject docInfo = pathInfo.getJSONObject(method);
                docInfo.putIfAbsent("real_req_path", apiPath);
                docInfo.put("basePath", basePath);
                DocItem docItem = buildDocItem(docInfo, docRoot);
                if (Objects.isNull(docItem)) {
                    continue;
                }
                if (docItem.isUploadRequest()) {
                    docItem.setHttpMethodList(Sets.newHashSet("post"));
                } else {
                    docItem.setHttpMethodList(httpMethodList);
                }
                docItems.add(docItem);
            }
        }

        docItems.sort(Comparator.comparing(DocItem::getApiOrder).thenComparing(DocItem::getName));

        List<DocModule> docModuleList = docItems.stream()
            .collect(Collectors.groupingBy(DocItem::getModule))
            .entrySet()
            .stream()
            .map(entry -> {
                List<DocItem> docItemList = entry.getValue();
                DocModule docModule = new DocModule();
                docModule.setModule(entry.getKey());
                docModule.setDocItems(docItemList);
                docModule.setOrder(getMuduleOrder(docItemList));
                return docModule;
            })
            .sorted(Comparator.comparing(DocModule::getOrder))
            .collect(Collectors.toList());

        DocInfo docInfo = new DocInfo();
        docInfo.setTitle(title);
        docInfo.setDocModuleList(docModuleList);
        return docInfo;
    }

    private int getMuduleOrder(final List<DocItem> items) {
        if (CollectionUtils.isEmpty(items)) {
            return Integer.MAX_VALUE;
        }
        List<DocItem> docItemList = new ArrayList<>(items);
        docItemList.sort(Comparator.comparing(DocItem::getModuleOrder));
        return docItemList.get(0).getModuleOrder();
    }

    protected Collection<String> getHttpMethods(final JSONObject pathInfo) {
        // key: get,post,head...
        List<String> retList;
        Set<String> httpMethodList = pathInfo.keySet();
        if (httpMethodList.size() <= 2) {
            retList = new ArrayList<>(httpMethodList);
        } else {
//            Set<String> ignoreHttpMethods = DocParserContext.ignoreHttpMethods;
//            retList = httpMethodList.stream()
//                    .filter(method -> !ignoreHttpMethods.contains(method.toLowerCase()))
//                    .collect(Collectors.toList());
            retList = new ArrayList<>(httpMethodList);
        }
        Collections.sort(retList);
        return retList;
    }

    protected DocItem buildDocItem(final JSONObject docInfo, final JSONObject docRoot) {
        String apiName = docInfo.getString("real_req_path");
        String basePath = docInfo.getString("basePath");
        apiName = basePath + apiName;

        DocItem docItem = new DocItem();
        docItem.setId(UUID.randomUUID().toString());
        docItem.setName(apiName);
        docItem.setSummary(docInfo.getString("summary"));
        docItem.setDescription(docInfo.getString("description"));
        docItem.setProduces(docInfo.getJSONArray("produces").toJavaList(String.class));
        docItem.setMultiple(docInfo.getString("multiple") != null);
        String apiResponseStr = docInfo.getString("apiResponse");
        if (apiResponseStr != null) {
            docItem.setBizCodeList(JSON.parseArray(apiResponseStr, CustomCode.class));
        }
        docItem.setModuleOrder(NumberUtils.toInt(docInfo.getString("module_order"), 0));
        docItem.setApiOrder(NumberUtils.toInt(docInfo.getString("api_order"), 0));
        String moduleName = this.buildModuleName(docInfo, docRoot, basePath);
        docItem.setModule(moduleName);
        List<DocParameter> docParameterList = this.buildRequestParameterList(docInfo, docRoot);
        docItem.setRequestParameters(docParameterList);

        List<DocParameter> responseParameterList = this.buildResponseParameterList(docInfo, docRoot);
        docItem.setResponseParameters(responseParameterList);
        return docItem;
    }

    protected String buildModuleName(final JSONObject docInfo, final JSONObject docRoot, final String basePath) {
        final String title = Optional.ofNullable(docRoot.getJSONObject("info")).map(jsonObject -> jsonObject.getString("title")).orElse(basePath);
        JSONArray tags = docInfo.getJSONArray("tags");
        if (Objects.nonNull(tags) && tags.size() > 0) {
            return tags.getString(0);
        }
        return title;
    }

    protected List<DocParameter> buildRequestParameterList(final JSONObject docInfo, final JSONObject docRoot) {
        Optional<JSONArray> parametersOptional = Optional.ofNullable(docInfo.getJSONArray("parameters"));
        JSONArray parameters = parametersOptional.orElse(new JSONArray());
        List<DocParameter> docParameterList = new ArrayList<>();
        for (int i = 0; i < parameters.size(); i++) {
            JSONObject fieldJson = parameters.getJSONObject(i);
            JSONObject schema = fieldJson.getJSONObject("schema");
            if (Objects.nonNull(schema)) {
                RefInfo refInfo = getRefInfo(schema);
                if (Objects.nonNull(refInfo)) {
                    List<DocParameter> parameterList = this.buildDocParameters(refInfo.ref, docRoot, true);
                    docParameterList.addAll(parameterList);
                }
            } else {
                DocParameter docParameter = fieldJson.toJavaObject(DocParameter.class);
                docParameterList.add(docParameter);
            }
        }

        Map<String, List<DocParameter>> collect = docParameterList.stream()
            .filter(docParameter -> docParameter.getName().contains("."))
            .map(docParameter -> {
                String name = docParameter.getName();
                int index = name.indexOf('.');
                String module = name.substring(0, index);
                String newName = name.substring(index + 1);
                DocParameter ret = new DocParameter();
                BeanUtils.copyProperties(docParameter, ret);
                ret.setName(newName);
                ret.setModule(module);
                return ret;
            })
            .collect(Collectors.groupingBy(DocParameter::getModule));

        collect.forEach((key, value) -> {
            DocParameter moduleDoc = new DocParameter();
            moduleDoc.setName(key);
            moduleDoc.setType("object");
            moduleDoc.setRefs(value);
            docParameterList.add(moduleDoc);
        });

        return docParameterList.stream()
            .filter(docParameter -> !docParameter.getName().contains("."))
            .collect(Collectors.toList());
    }

    protected List<DocParameter> buildResponseParameterList(final JSONObject docInfo, final JSONObject docRoot) {
        RefInfo refInfo = getResponseRefInfo(docInfo);
        List<DocParameter> respParameterList = Collections.emptyList();
        if (refInfo != null) {
            String responseRef = refInfo.ref;
            respParameterList = this.buildDocParameters(responseRef, docRoot, true);
            // If an array is returned.
            if (refInfo.isArray) {
                DocParameter docParameter = new DocParameter();
                docParameter.setName("items");
                docParameter.setType("array");
                docParameter.setRefs(respParameterList);
                respParameterList = Collections.singletonList(docParameter);
            }
        }
        return respParameterList;
    }

    protected List<DocParameter> buildDocParameters(final String ref, final JSONObject docRoot, final boolean doSubRef) {
        JSONObject responseObject = docRoot.getJSONObject("definitions").getJSONObject(ref);
        String className = responseObject.getString("title");
        JSONObject extProperties = docRoot.getJSONObject(className);
        JSONArray requiredProperties = responseObject.getJSONArray("required");
        JSONObject properties = responseObject.getJSONObject("properties");
        List<DocParameter> docParameterList = new ArrayList<>();
        if (Objects.isNull(properties)) {
            return docParameterList;
        }
        Set<String> fieldNames = properties.keySet();
        for (String fieldName : fieldNames) {
            JSONObject fieldInfo = properties.getJSONObject(fieldName);
            DocParameter docParameter = fieldInfo.toJavaObject(DocParameter.class);
            docParameter.setName(fieldName);
            docParameter.setRequired(
                !CollectionUtils.isEmpty(requiredProperties) && requiredProperties.contains(fieldName));
            if (Objects.nonNull(extProperties)) {
                JSONObject prop = extProperties.getJSONObject(fieldName);
                if (Objects.nonNull(prop)) {
                    String maxLength = prop.getString("maxLength");
                    docParameter.setMaxLength(Objects.isNull(maxLength) ? "-" : maxLength);
                    String required = prop.getString("required");
                    if (Objects.nonNull(required)) {
                        docParameter.setRequired(Boolean.parseBoolean(required));
                    }
                }
            }
            docParameterList.add(docParameter);
            RefInfo refInfo = this.getRefInfo(fieldInfo);
            if (Objects.nonNull(refInfo) && doSubRef) {
                String subRef = refInfo.ref;
                boolean nextDoRef = !Objects.equals(ref, subRef);
                List<DocParameter> refs = buildDocParameters(subRef, docRoot, nextDoRef);
                docParameter.setRefs(refs);
            }
        }
        return docParameterList;
    }

    /**
     * Simple object return, pure array return.

     * @param docInfo docInfo
     * @return RefInfo
     */
    protected RefInfo getResponseRefInfo(final JSONObject docInfo) {
        return Optional.ofNullable(docInfo.getJSONObject("responses"))
            .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getJSONObject("200")))
            .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getJSONObject("schema")))
            .map(this::getRefInfo)
            .orElse(null);
    }

    private RefInfo getRefInfo(final JSONObject jsonObject) {
        String ref;
        boolean isArray = "array".equals(jsonObject.getString("type"));
        if (isArray) {
            ref = jsonObject.getJSONObject("items").getString("$ref");
        } else {
            // #/definitions/xxx
            ref = jsonObject.getString("$ref");
        }
        if (Objects.isNull(ref)) {
            return null;
        }
        int index = ref.lastIndexOf("/");
        if (index > -1) {
            ref = ref.substring(index + 1);
        }
        RefInfo refInfo = new RefInfo();
        refInfo.isArray = isArray;
        refInfo.ref = ref;
        return refInfo;
    }

    private static class RefInfo {

        private boolean isArray;

        private String ref;
    }

}
