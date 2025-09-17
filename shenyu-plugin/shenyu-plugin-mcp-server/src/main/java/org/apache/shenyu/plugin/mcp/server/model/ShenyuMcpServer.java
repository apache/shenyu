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

package org.apache.shenyu.plugin.mcp.server.model;

import java.util.Objects;

/**
 * McpToolDescription represents a tool description in the context of a microservice.
 * It contains information about the tool's ID, name, version, description, parameters,
 * and return type.
 */
public class ShenyuMcpServer {
    
    private String path;
    
    private String messageEndpoint;
    
    public String getPath() {
        return path;
    }
    
    public void setPath(final String path) {
        this.path = path;
    }
    
    public String getMessageEndpoint() {
        return messageEndpoint;
    }
    
    public void setMessageEndpoint(final String messageEndpoint) {
        this.messageEndpoint = messageEndpoint;
    }
    
    public static ShenyuMcpServer newInstance() {
        ShenyuMcpServer shenyuMcpServer = new ShenyuMcpServer();
        shenyuMcpServer.setPath("");
        shenyuMcpServer.setMessageEndpoint("/message");
        return shenyuMcpServer;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ShenyuMcpServer that = (ShenyuMcpServer) o;
        return Objects.equals(path, that.path)
                && Objects.equals(messageEndpoint, that.messageEndpoint);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(path, messageEndpoint);
    }
    
    @Override
    public String toString() {
        return String.format("McpServerPluginRuleHandle: path: %s, messageEndpoint: %s",
                path,
                messageEndpoint);
    }
}
