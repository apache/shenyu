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

import com.google.common.collect.Sets;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.shenyu.admin.model.bean.CustomCode;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.bean.DocModule;
import org.apache.shenyu.admin.model.bean.DocParameter;
import org.apache.shenyu.admin.service.manager.DocParser;
import org.apache.shenyu.common.enums.SwaggerVersion;
import org.apache.shenyu.common.utils.GsonUtils;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

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
    public DocInfo parseJson(final JsonObject docRoot) {
        // Detect Swagger version
        SwaggerVersion version = detectSwaggerVersion(docRoot);
        
        final String basePath = extractBasePath(docRoot, version);
        final String title = Optional.ofNullable(docRoot.getAsJsonObject("info")).map(jsonObject -> jsonObject.get("title").getAsString()).orElse(basePath);
        final List<DocItem> docItems = new ArrayList<>();

        JsonObject paths = docRoot.getAsJsonObject("paths");
        if (Objects.isNull(paths)) {
            paths = new JsonObject();
        }
        Set<String> pathNameSet = paths.keySet();
        for (String apiPath : pathNameSet) {
            JsonObject pathInfo = paths.getAsJsonObject(apiPath);
            Collection<String> httpMethodList = getHttpMethods(pathInfo);
            Optional<String> first = httpMethodList.stream().findFirst();
            if (first.isPresent()) {
                String method = first.get();
                JsonObject docInfo = pathInfo.getAsJsonObject(method);
                docInfo.addProperty("real_req_path", apiPath);
                docInfo.addProperty("basePath", basePath);
                DocItem docItem = buildDocItem(docInfo, docRoot, version);
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

    protected Collection<String> getHttpMethods(final JsonObject pathInfo) {
        Set<String> httpMethodList = pathInfo.keySet();
        List<String> retList = new ArrayList<>(httpMethodList);
        Collections.sort(retList);
        return retList;
    }

    protected DocItem buildDocItem(final JsonObject docInfo, final JsonObject docRoot, final SwaggerVersion version) {
        String apiName = docInfo.get("real_req_path").getAsString();
        String basePath = docInfo.get("basePath").getAsString();
        apiName = basePath + apiName;

        DocItem docItem = new DocItem();
        docItem.setName(apiName);
        if (Objects.nonNull(docInfo.get("summary"))) {
            docItem.setSummary(docInfo.get("summary").getAsString());
        }
        if (Objects.nonNull(docInfo.get("description"))) {
            docItem.setDescription(docInfo.get("description").getAsString());
        }
        docItem.setConsumes(GsonUtils.getGson().fromJson(docInfo.getAsJsonArray("consumes"), new TypeToken<List<String>>() {
        }.getType()));
        docItem.setProduces(GsonUtils.getGson().fromJson(docInfo.getAsJsonArray("produces"), new TypeToken<List<String>>() {
        }.getType()));

        if (Objects.nonNull(docInfo.get("multiple"))) {
            docItem.setMultiple(true);
        }
        if (Objects.nonNull(docInfo.get("apiResponse"))) {
            docItem.setBizCodeList(GsonUtils.getInstance().fromList(docInfo.get("apiResponse").getAsString(), CustomCode.class));
        }
        if (Objects.nonNull(docInfo.get("module_order"))) {
            docItem.setModuleOrder(NumberUtils.toInt(docInfo.get("module_order").getAsString(), 0));
        }
        if (Objects.nonNull(docInfo.get("api_order"))) {
            docItem.setApiOrder(NumberUtils.toInt(docInfo.get("api_order").getAsString(), 0));
        }
        String moduleName = this.buildModuleName(docInfo, docRoot, basePath);
        docItem.setModule(moduleName);
        this.buildRequestParameterList(docItem, docInfo, docRoot, version);
        List<DocParameter> responseParameterList = this.buildResponseParameterList(docInfo, docRoot, version);
        docItem.setResponseParameters(responseParameterList);
        return docItem;
    }

    protected String buildModuleName(final JsonObject docInfo, final JsonObject docRoot, final String basePath) {
        JsonArray tags = docInfo.getAsJsonArray("tags");
        if (Objects.nonNull(tags) && !tags.isEmpty()) {
            return tags.get(0).getAsString();
        }
        return Optional.ofNullable(docRoot.getAsJsonObject("info")).map(jsonObject -> jsonObject.get("title").getAsString()).orElse(basePath);
    }

    protected void buildRequestParameterList(final DocItem docItem, final JsonObject docInfo,
                                           final JsonObject docRoot, final SwaggerVersion version) {
        Optional<JsonArray> parametersOptional = Optional.ofNullable(docInfo.getAsJsonArray("parameters"));
        JsonArray parameters = parametersOptional.orElse(new JsonArray());
        List<DocParameter> docRequestParameterList = new ArrayList<>();
        List<DocParameter> docHeaderParameterList = new ArrayList<>();
        
        for (int i = 0; i < parameters.size(); i++) {
            JsonObject fieldJson = parameters.get(i).getAsJsonObject();
            JsonObject schema = fieldJson.getAsJsonObject("schema");
            
            if (Objects.nonNull(schema)) {
                RefInfo refInfo = getRefInfo(schema, version);
                if (Objects.nonNull(refInfo)) {
                    List<DocParameter> parameterList = this.buildDocParameters(refInfo.ref, docRoot, true, version);
                    docRequestParameterList.addAll(parameterList);
                }
            } else {
                DocParameter docParameter = GsonUtils.getInstance().fromJson(fieldJson, DocParameter.class);
                JsonElement inElement = fieldJson.get("in");
                if (Objects.nonNull(inElement) && "header".equals(inElement.getAsString())) {
                    docHeaderParameterList.add(docParameter);
                } else {
                    docRequestParameterList.add(docParameter);
                }
            }
        }

        Map<String, List<DocParameter>> collect = docRequestParameterList.stream()
            .filter(docParameter -> docParameter.getName().contains("."))
            .map(docParameter -> {
                String name = docParameter.getName();
                int index = name.indexOf('.');
                String module = name.substring(0, index);
                String newName = name.substring(index + 1);
                DocParameter ret = DocParameter.copy(docParameter);
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
            docRequestParameterList.add(moduleDoc);
        });

        List<DocParameter> requestParameterList = docRequestParameterList.stream()
            .filter(docParameter -> !docParameter.getName().contains("."))
            .collect(Collectors.toList());

        docItem.setRequestParameters(requestParameterList);
        docItem.setRequestHeaders(docHeaderParameterList);
    }

    protected List<DocParameter> buildResponseParameterList(final JsonObject docInfo, 
                                                          final JsonObject docRoot, 
                                                          final SwaggerVersion version) {
        RefInfo refInfo = getResponseRefInfo(docInfo, version);
        List<DocParameter> respParameterList = Collections.emptyList();
        
        if (Objects.nonNull(refInfo)) {
            String responseRef = refInfo.ref;
            respParameterList = this.buildDocParameters(responseRef, docRoot, true, version);
            
            // If it returns an array
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

    protected List<DocParameter> buildDocParameters(final String ref, final JsonObject docRoot, 
                                                   final boolean doSubRef, final SwaggerVersion version) {
        JsonObject schemaDefinitions = getSchemaDefinitions(docRoot, version);
        if (Objects.isNull(schemaDefinitions)) {
            return Collections.emptyList();
        }
        
        JsonObject responseObject = schemaDefinitions.getAsJsonObject(ref);
        if (Objects.isNull(responseObject)) {
            return Collections.emptyList();
        }
        
        JsonObject properties = responseObject.getAsJsonObject("properties");
        JsonArray requiredProperties = responseObject.getAsJsonArray("required");
        List<String> requiredFieldList = this.jsonArrayToStringList(requiredProperties);
        List<DocParameter> docParameterList = new ArrayList<>();
        
        if (Objects.isNull(properties)) {
            return docParameterList;
        }
        
        Set<String> fieldNames = properties.keySet();
        for (String fieldName : fieldNames) {
            JsonObject fieldInfo = properties.getAsJsonObject(fieldName);
            DocParameter docParameter = GsonUtils.getInstance().fromJson(fieldInfo, DocParameter.class);
            docParameter.setName(fieldName);
            docParameter.setRequired(requiredFieldList.contains(fieldName));

            JsonObject prop = properties.getAsJsonObject(fieldName);
            if (Objects.nonNull(prop)) {
                docParameter.setMaxLength(Objects.isNull(prop.get("maxLength")) ? "-" : prop.get("maxLength").getAsString());
                if (Objects.nonNull(prop.get("required"))) {
                    docParameter.setRequired(Boolean.parseBoolean(prop.get("required").getAsString()));
                }
            }

            docParameterList.add(docParameter);
            RefInfo refInfo = this.getRefInfo(fieldInfo, version);
            if (Objects.nonNull(refInfo) && doSubRef) {
                String subRef = refInfo.ref;
                boolean nextDoRef = !Objects.equals(ref, subRef);
                List<DocParameter> refs = buildDocParameters(subRef, docRoot, nextDoRef, version);
                docParameter.setRefs(refs);
            }
        }
        return docParameterList;
    }

    private List<String> jsonArrayToStringList(final JsonArray jsonArray) {
        if (Objects.isNull(jsonArray)) {
            return Collections.emptyList();
        }
        List<String> list = new ArrayList<>(jsonArray.size());
        for (JsonElement jsonElement : jsonArray) {
            if (jsonElement.isJsonNull()) {
                continue;
            }
            String objStr = jsonElement.getAsString();
            list.add(objStr);
        }
        return list;
    }

    /**
     * Simple object return, pure array return.
     *
     * @param docInfo docInfo
     * @param version version
     * @return RefInfo
     */
    protected RefInfo getResponseRefInfo(final JsonObject docInfo, final SwaggerVersion version) {
        if (version == SwaggerVersion.V2) {
            // v2: responses/200/schema
            return Optional.ofNullable(docInfo.getAsJsonObject("responses"))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("200")))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("schema")))
                .map(schema -> this.getRefInfo(schema, version))
                .orElse(null);
        } else {
            // v3: responses/200/content/application/json/schema
            return Optional.ofNullable(docInfo.getAsJsonObject("responses"))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("200")))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("content")))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("application/json")))
                .flatMap(jsonObject -> Optional.ofNullable(jsonObject.getAsJsonObject("schema")))
                .map(schema -> this.getRefInfo(schema, version))
                .orElse(null);
        }
    }

    private RefInfo getRefInfo(final JsonObject jsonObject, final SwaggerVersion version) {
        JsonElement refElement;
        boolean isArray = Objects.nonNull(jsonObject.get("type")) && "array".equals(jsonObject.get("type").getAsString());
        
        if (isArray) {
            refElement = jsonObject.getAsJsonObject("items").get("$ref");
        } else {
            refElement = jsonObject.get("$ref");
        }
        
        if (Objects.isNull(refElement)) {
            return null;
        }
        
        String ref = refElement.getAsString();
        
        // Parse reference path
        if (version == SwaggerVersion.V2) {
            // v2: #/definitions/ModelName
            if (ref.startsWith("#/definitions/")) {
                ref = ref.substring("#/definitions/".length());
            }
        } else {
            // v3: #/components/schemas/ModelName
            if (ref.startsWith("#/components/schemas/")) {
                ref = ref.substring("#/components/schemas/".length());
            }
        }
        
        RefInfo refInfo = new RefInfo();
        refInfo.isArray = isArray;
        refInfo.ref = ref;
        return refInfo;
    }

    /**
     * Extract base path based on Swagger version.
     *
     * @param docRoot docRoot
     * @param version version
     * @return base path
     */
    private String extractBasePath(final JsonObject docRoot, final SwaggerVersion version) {
        if (version == SwaggerVersion.V2) {
            // Swagger 2.0: use basePath field
            return Optional.ofNullable(docRoot.get("basePath"))
                .map(JsonElement::getAsString)
                .orElse("/");
        } else {
            // OpenAPI 3.0: use servers[0].url
            return Optional.ofNullable(docRoot.getAsJsonArray("servers"))
                .filter(servers -> !servers.isEmpty())
                .map(servers -> servers.get(0).getAsJsonObject())
                .map(server -> server.get("url"))
                .map(JsonElement::getAsString)
                .map(this::extractPathFromUrl)
                .orElse("/");
        }
    }

    /**
     * Extract path from server URL.
     * For example: "https://api.example.com/v1" -> "/v1"
     *
     * @param url server URL
     * @return path part of URL
     */
    private String extractPathFromUrl(final String url) {
        if (Objects.isNull(url) || url.trim().isEmpty()) {
            return "/";
        }
        
        try {
            // Handle relative URLs
            if (url.startsWith("/")) {
                return url;
            }
            
            // Handle absolute URLs
            URL parsedUrl = new URL(url);
            String path = parsedUrl.getPath();
            return Objects.isNull(path) || path.trim().isEmpty() ? "/" : path;
        } catch (Exception e) {
            // If URL parsing fails, try to extract path manually
            int protocolIndex = url.indexOf("://");
            if (protocolIndex != -1) {
                String afterProtocol = url.substring(protocolIndex + 3);
                int pathIndex = afterProtocol.indexOf("/");
                if (pathIndex != -1) {
                    return afterProtocol.substring(pathIndex);
                }
            }
            return "/";
        }
    }

    /**
     * Detect Swagger version.
     *
     * @param docRoot docRoot
     * @return SwaggerVersion
     */
    private SwaggerVersion detectSwaggerVersion(final JsonObject docRoot) {
        // Check if openapi field exists (v3)
        if (docRoot.has("openapi")) {
            return SwaggerVersion.V3;
        }
        
        // Check if swagger field exists with value 2.0 (v2)
        if (docRoot.has("swagger")) {
            String swaggerVersion = docRoot.get("swagger").getAsString();
            if (swaggerVersion.startsWith("2.")) {
                return SwaggerVersion.V2;
            }
        }
        
        // Default to v3
        return SwaggerVersion.V3;
    }

    /**
     * Get schema definitions by version.
     *
     * @param docRoot docRoot
     * @param version version
     * @return JsonObject
     */
    private JsonObject getSchemaDefinitions(final JsonObject docRoot, final SwaggerVersion version) {
        if (version == SwaggerVersion.V2) {
            return docRoot.getAsJsonObject("definitions");
        } else {
            JsonObject components = docRoot.getAsJsonObject("components");
            if (Objects.isNull(components)) {
                return null;
            }
            return components.getAsJsonObject("schemas");
        }
    }

    private static class RefInfo {

        private boolean isArray;

        private String ref;
    }

}
