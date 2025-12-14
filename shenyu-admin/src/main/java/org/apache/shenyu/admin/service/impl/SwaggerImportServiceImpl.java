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

package org.apache.shenyu.admin.service.impl;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.parser.OpenAPIV3Parser;
import okhttp3.Response;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;
import org.apache.shenyu.admin.service.SwaggerImportService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterMcpServiceImpl;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.admin.utils.UrlSecurityUtils;
import org.apache.shenyu.client.mcp.common.dto.ShenyuMcpRequestConfig;
import org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool;
import org.apache.shenyu.client.mcp.generator.McpToolsRegisterDTOGenerator;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SwaggerImportService}.
 */
@Service
public class SwaggerImportServiceImpl implements SwaggerImportService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SwaggerImportServiceImpl.class);

    private final DocManager docManager;
    
    private final HttpUtils httpUtils;

    @Resource
    private ShenyuClientRegisterMcpServiceImpl shenyuClientRegisterMcpService;

    public SwaggerImportServiceImpl(final DocManager docManager, final HttpUtils httpUtils) {
        this.docManager = docManager;
        this.httpUtils = httpUtils;
    }
    
    @Override
    public String importSwagger(final SwaggerImportRequest request) {
        LOG.info("Start importing Swagger document: {}", request);
        
        try {
            // 1. Validate URL
            validateSwaggerUrl(request.getSwaggerUrl());
            
            // 2. Get swagger document
            String swaggerJson = fetchSwaggerDoc(request.getSwaggerUrl());
            
            // 3. Validate Swagger content and version
            validateSwaggerContent(swaggerJson);
            
            // 4. Create virtual instance
            UpstreamInstance instance = createVirtualInstance(request);
            
            // 5. Parse and save document
            docManager.addDocInfo(instance, swaggerJson, null, docInfo -> {
                LOG.info("Successfully imported swagger document: {} with MD5: {}", 
                    request.getProjectName(), docInfo.getDocMd5());
            });
            
            return "Import successful, supports Swagger 2.0 and OpenAPI 3.0 formats";
            
        } catch (Exception e) {
            LOG.error("Failed to import swagger document: {}", request.getProjectName(), e);
            throw new RuntimeException("Import failed: " + e.getMessage(), e);
        }
    }

    @Override
    public String importMcpConfig(final SwaggerImportRequest request) {
        LOG.info("Start importing Mcp config: {}", request);

        try {
            validateSwaggerUrl(request.getSwaggerUrl());

            String swaggerJson = fetchSwaggerDoc(request.getSwaggerUrl());

            List<McpToolsRegisterDTO> mcpToolsRegisterDTOList = buildMcpToolRegisterDTO(swaggerJson, request.getNamespaceId());

            mcpToolsRegisterDTOList.forEach(mcpToolsRegisterDTO -> {
                shenyuClientRegisterMcpService.registerMcpTools(mcpToolsRegisterDTO);
            });

            return "Import mcp server config successful, supports Swagger 2.0 and OpenAPI 3.0 formats";
        } catch (IOException e) {
            LOG.error("Failed to import mcp config: {}", request.getProjectName(), e);
            throw new RuntimeException("Import mcp server config failed: " + e.getMessage(), e);
        }

    }

    private List<McpToolsRegisterDTO> buildMcpToolRegisterDTO(final String swaggerJson, final String namespaceId) {
        ArrayList<McpToolsRegisterDTO> mcpToolsRegisterDTOList = new ArrayList<>();
        OpenAPI openapi = new OpenAPIV3Parser().readContents(swaggerJson, null, null).getOpenAPI();
        Map<String, List<Map<String, ShenyuMcpTool>>> mcpToolMap = buildShenyuMcpTool(openapi);
        JsonObject openApiJsonObject = JsonParser.parseString(swaggerJson).getAsJsonObject();
        mcpToolMap.forEach((selectorName, mcpToolList) -> {
            mcpToolList.forEach(shenyuMcpToolMap -> {
                shenyuMcpToolMap.forEach((url, shenyuMcpTool) -> {
                    McpToolsRegisterDTO mcpToolsRegisterDTO = McpToolsRegisterDTOGenerator.generateRegisterDTO(shenyuMcpTool, openApiJsonObject, url, namespaceId);

                    mcpToolsRegisterDTO.setMetaDataRegisterDTO(buildMetaDataRegisterDTO(openapi, selectorName, shenyuMcpTool, url, namespaceId));
                    mcpToolsRegisterDTOList.add(mcpToolsRegisterDTO);
                });
            });
        });
        return mcpToolsRegisterDTOList;
    }

    private Map<String, List<Map<String, ShenyuMcpTool>>> buildShenyuMcpTool(final OpenAPI openapi) {
        if (Objects.isNull(openapi) || Objects.isNull(openapi.getPaths())) {
            return Collections.emptyMap();
        }
        Map<String, List<Map<String, ShenyuMcpTool>>> result = new HashMap<>();
        Paths paths = openapi.getPaths();

        for (Map.Entry<String, PathItem> entry : paths.entrySet()) {
            String fullPath = entry.getKey();
            PathItem pathItem = entry.getValue();

            String[] segments = fullPath.split("/");
            String mainPath = Arrays.stream(segments)
                    .filter(s -> !s.isEmpty())
                    .findFirst()
                    .orElse("default");

            List<Map<String, ShenyuMcpTool>> maps = result.computeIfAbsent(mainPath, k -> new ArrayList<>());
            Map<String, ShenyuMcpTool> toolMap = new HashMap<>();

            Map<PathItem.HttpMethod, Operation> operationsMap = pathItem.readOperationsMap();
            for (Map.Entry<PathItem.HttpMethod, Operation> opEntry : operationsMap.entrySet()) {

                Operation operation = opEntry.getValue();

                ShenyuMcpTool tool = new ShenyuMcpTool();
                tool.setOperation(operation);
                ShenyuMcpRequestConfig shenyuMcpRequestConfig = new ShenyuMcpRequestConfig();
                shenyuMcpRequestConfig.setBodyToJson("false");
                tool.setRequestConfig(shenyuMcpRequestConfig);
                tool.setToolName(operation.getOperationId());
                tool.setEnable(true);
                PathItem.HttpMethod httpMethod = opEntry.getKey();
                tool.setMethod(httpMethod.name().toLowerCase());

                toolMap.put(fullPath, tool);
            }
            maps.add(toolMap);
        }
        return result;
    }

    private MetaDataRegisterDTO buildMetaDataRegisterDTO(final OpenAPI openapi, final String selectorName,
                                                         final ShenyuMcpTool shenyuMcpTool, final String contentPath,
                                                         final String namespaceId) {
        String urlString = openapi.getServers().get(0).getUrl();
        URL url;
        try {
            url = new URL(urlString);
        } catch (MalformedURLException e) {
            LOG.error("url error");
            throw new RuntimeException(e);
        }
        String host = url.getHost();
        int port = url.getPort();
        Operation operation = shenyuMcpTool.getOperation();
        String parameterTypes = operation.getParameters()
                .stream()
                .map(Parameter::getIn)
                .collect(Collectors.joining(","));

        return MetaDataRegisterDTO.builder()
                .appName(openapi.getInfo().getTitle())
                .serviceName(selectorName)
                .methodName(shenyuMcpTool.getToolName())
                .contextPath(selectorName)
                .path(contentPath)
                .port(port)
                .host(host)
                .ruleName(shenyuMcpTool.getToolName())
                .pathDesc(operation.getDescription())
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.MCP.getName())
                .namespaceId(namespaceId)
                .build();
    }

    @Override
    public boolean testConnection(final String swaggerUrl) {
        try {
            validateSwaggerUrl(swaggerUrl);
            try (Response response = httpUtils.requestForResponse(swaggerUrl, 
                    Collections.emptyMap(), Collections.emptyMap(), HttpUtils.HTTPMethod.GET)) {
                return response.code() == 200;
            }
        } catch (Exception e) {
            LOG.warn("Failed to test Swagger URL connection: {}", swaggerUrl, e);
            return false;
        }
    }
    
    private void validateSwaggerUrl(final String swaggerUrl) {
        // Use UrlSecurityUtils for SSRF protection
        UrlSecurityUtils.validateUrlForSSRF(swaggerUrl);
    }
    
    private String fetchSwaggerDoc(final String swaggerUrl) throws IOException {
        try (Response response = httpUtils.requestForResponse(swaggerUrl,
                Collections.emptyMap(), Collections.emptyMap(), HttpUtils.HTTPMethod.GET)) {
            
            if (response.code() != 200) {
                throw new RuntimeException("Failed to get Swagger document, HTTP status code: " + response.code());
            }
            
            return response.body().string();
        }
    }
    
    private void validateSwaggerContent(final String swaggerJson) {
        try {
            JsonObject docRoot = GsonUtils.getInstance().fromJson(swaggerJson, JsonObject.class);
            
            // Detect version
            boolean isV2 = docRoot.has("swagger") && docRoot.get("swagger").getAsString().startsWith("2.");
            boolean isV3 = docRoot.has("openapi") && docRoot.get("openapi").getAsString().startsWith("3.");
            
            if (!isV2 && !isV3) {
                throw new IllegalArgumentException("Unsupported Swagger version, only Swagger 2.0 and OpenAPI 3.0 formats are supported");
            }
            
            LOG.info("Detected Swagger version: {}", isV2 ? "2.0" : "3.0");
            
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid Swagger JSON format: " + e.getMessage());
        }
    }
    
    private UpstreamInstance createVirtualInstance(final SwaggerImportRequest request) {
        UpstreamInstance instance = new UpstreamInstance();
        instance.setContextPath(request.getProjectName());
        
        // Try to parse IP and port from URL
        try {
            URL url = new URL(request.getSwaggerUrl());
            instance.setIp(url.getHost());
            instance.setPort(url.getPort() == -1 ? (url.getProtocol().equals("https") ? 443 : 80) : url.getPort());
        } catch (Exception e) {
            instance.setIp("unknown");
            instance.setPort(80);
        }
        
        return instance;
    }
} 
