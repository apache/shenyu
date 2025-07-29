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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;

/**
 * Swagger Import Service.
 */
public interface SwaggerImportService {
    
    /**
     * Import swagger documentation.
     *
     * @param request swagger import request
     * @return import result message
     */
    String importSwagger(SwaggerImportRequest request);
    
    /**
     * Test connection to swagger URL.
     *
     * @param swaggerUrl swagger URL to test
     * @return true if connection is successful, false otherwise
     */
    boolean testConnection(String swaggerUrl);
} 