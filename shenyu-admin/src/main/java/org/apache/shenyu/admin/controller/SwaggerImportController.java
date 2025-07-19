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

package org.apache.shenyu.admin.controller;

import jakarta.validation.Valid;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;
import org.apache.shenyu.admin.service.SwaggerImportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Swagger Import Controller.
 */
@RestController
@RequestMapping("/swagger")
@Validated
public class SwaggerImportController {

    private static final Logger LOG = LoggerFactory.getLogger(SwaggerImportController.class);

    private final SwaggerImportService swaggerImportService;

    public SwaggerImportController(final SwaggerImportService swaggerImportService) {
        this.swaggerImportService = swaggerImportService;
    }

    /**
     * Import swagger documentation.
     *
     * @param request the swagger import request
     * @return the result of swagger import
     */
    @PostMapping("/import")
    public ShenyuAdminResult importSwagger(@Valid @RequestBody final SwaggerImportRequest request) {
        LOG.info("Received Swagger import request: {}", request);

        try {
            String result = swaggerImportService.importSwagger(request);
            return ShenyuAdminResult.success(result);

        } catch (Exception e) {
            LOG.error("Failed to import swagger document", e);

            return ShenyuAdminResult.error("Import failed: " + e.getMessage());
        }
    }

    /**
     * Test connection to swagger URL.
     *
     * @param swaggerUrl the swagger URL to test
     * @return the result of connection test
     */
    @PostMapping("/test-connection")
    public ShenyuAdminResult testConnection(@RequestParam final String swaggerUrl) {
        LOG.info("Testing Swagger URL connection: {}", swaggerUrl);

        try {
            boolean isConnected = swaggerImportService.testConnection(swaggerUrl);

            return ShenyuAdminResult.success(isConnected ? "Connection successful" : "Connection failed");

        } catch (Exception e) {
            LOG.error("Failed to test connection", e);

            return ShenyuAdminResult.error("Connection failed: " + e.getMessage());
        }
    }
} 