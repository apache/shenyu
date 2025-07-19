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
import okhttp3.Response;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;
import org.apache.shenyu.admin.service.SwaggerImportService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.admin.utils.UrlSecurityUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SwaggerImportService}.
 */
@Service
public class SwaggerImportServiceImpl implements SwaggerImportService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SwaggerImportServiceImpl.class);

    private final DocManager docManager;
    
    private final HttpUtils httpUtils;
    
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