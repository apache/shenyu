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

package org.apache.shenyu.common.enums;

/**
 * Swagger version enumeration.
 */
public enum SwaggerVersion {
    
    /**
     * Swagger 2.0.
     */
    V2("2.0"),
    
    /**
     * OpenAPI 3.0.
     */
    V3("3.0");
    
    private final String version;
    
    SwaggerVersion(final String version) {
        this.version = version;
    }
    
    /**
     * get version.
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }
} 