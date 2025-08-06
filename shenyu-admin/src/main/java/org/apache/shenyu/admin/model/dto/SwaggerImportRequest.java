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

package org.apache.shenyu.admin.model.dto;


import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;

/**
 * Swagger Import Request.
 */
public class SwaggerImportRequest {
    
    @NotBlank(message = "swagger URL cannot be empty")
    @Pattern(regexp = "^https?://.*", message = "swagger URL must be a valid HTTP/HTTPS address")
    private String swaggerUrl;
    
    @NotBlank(message = "project name cannot be empty")
    private String projectName;
    
    private String projectDescription;
    
    public String getSwaggerUrl() {
        return swaggerUrl;
    }
    
    public void setSwaggerUrl(final String swaggerUrl) {
        this.swaggerUrl = swaggerUrl;
    }
    
    public String getProjectName() {
        return projectName;
    }
    
    public void setProjectName(final String projectName) {
        this.projectName = projectName;
    }
    
    public String getProjectDescription() {
        return projectDescription;
    }
    
    public void setProjectDescription(final String projectDescription) {
        this.projectDescription = projectDescription;
    }
    
    @Override
    public String toString() {
        return "SwaggerImportRequest{"
                + "swaggerUrl='" + swaggerUrl + '\''
                + ", projectName='" + projectName + '\''
                + ", projectDescription='" + projectDescription + '\''
                + '}';
    }
} 