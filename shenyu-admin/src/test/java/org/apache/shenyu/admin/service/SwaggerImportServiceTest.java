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

import org.apache.shenyu.admin.service.impl.SwaggerImportServiceImpl;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.apache.shenyu.admin.service.manager.DocManager;

import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Test for SwaggerImportService.
 */
@ExtendWith(MockitoExtension.class)
public class SwaggerImportServiceTest {
    
    @Mock
    private DocManager docManager;
    
    @Mock
    private HttpUtils httpUtils;
    
    @Test
    public void testConnection() {
        SwaggerImportService service = new SwaggerImportServiceImpl(docManager, httpUtils);
        
        // Test invalid URLs
        assertFalse(service.testConnection("invalid-url"));
        assertFalse(service.testConnection(""));
        assertFalse(service.testConnection(null));
        
        // Test valid URL format but may fail to connect
        assertFalse(service.testConnection("http://invalid.example.com/swagger.json"));
    }
} 