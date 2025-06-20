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

package org.apache.shenyu.plugin.mcp.server.request;

import com.google.gson.JsonObject;

public class RequestConfig {
    
    private String method;
    
    private String path;
    
    private JsonObject bodyJson;
    
    private JsonObject requestTemplate;
    
    private boolean argsToJsonBody;
    
    public RequestConfig(final String method, final String path, final JsonObject bodyJson, final JsonObject requestTemplate, final boolean argsToJsonBody) {
        this.method = method;
        this.path = path;
        this.bodyJson = bodyJson;
        this.requestTemplate = requestTemplate;
        this.argsToJsonBody = argsToJsonBody;
    }
    
    public String getMethod() {
        return method;
    }
    
    public void setMethod(final String method) {
        this.method = method;
    }
    
    public String getPath() {
        return path;
    }
    
    public void setPath(final String path) {
        this.path = path;
    }
    
    public JsonObject getBodyJson() {
        return bodyJson;
    }
    
    public void setBodyJson(final JsonObject bodyJson) {
        this.bodyJson = bodyJson;
    }
    
    public JsonObject getRequestTemplate() {
        return requestTemplate;
    }
    
    public void setRequestTemplate(final JsonObject requestTemplate) {
        this.requestTemplate = requestTemplate;
    }
    
    public boolean isArgsToJsonBody() {
        return argsToJsonBody;
    }
    
    public void setArgsToJsonBody(final boolean argsToJsonBody) {
        this.argsToJsonBody = argsToJsonBody;
    }
}
