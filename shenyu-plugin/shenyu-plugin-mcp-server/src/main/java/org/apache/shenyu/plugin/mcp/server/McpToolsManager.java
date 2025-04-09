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

package org.apache.shenyu.plugin.mcp.server;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.mcp.McpTool;
import org.apache.shenyu.common.dto.mcp.McpToolParameter;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

public class McpToolsManager {
    
    private static final Logger LOG = LoggerFactory.getLogger(McpToolsManager.class);
    
    private final Map<String, McpTool> toolsRegistry = new ConcurrentHashMap<>();
    
    /**
     * 注册一个MCP工具.
     *
     * @param metaData 元数据
     */
    public void registerTool(final MetaData metaData) {
        try {
            McpTool tool = convertMetaDataToTool(metaData);
            toolsRegistry.put(tool.getName(), tool);
            LOG.info("Registered new MCP tool: {}", tool.getName());
        } catch (Exception e) {
            LOG.error("Failed to register MCP tool for metadata: {}", metaData.getPath(), e);
        }
    }
    
    /**
     * 注销一个MCP工具.
     *
     * @param metaData 元数据
     */
    public void unregisterTool(final MetaData metaData) {
        String toolName = generateToolName(metaData);
        if (Objects.nonNull(toolsRegistry.remove(toolName))) {
            LOG.info("Unregistered MCP tool: {}", toolName);
        }
    }
    
    /**
     * 获取指定工具.
     *
     * @return 工具实例
     */
    public List<McpTool> getAllTools() {
        return new ArrayList<>(toolsRegistry.values());
    }
    
    /**
     * 获取指定工具.
     *
     * @param metaData 元数据
     * @return 工具实例
     */
    private McpTool convertMetaDataToTool(final MetaData metaData) {
        String toolName = generateToolName(metaData);
        
        McpTool.Builder builder = McpTool.builder()
                .name(toolName)
                .description("Access " + metaData.getServiceName() + " via ShenYu Gateway")
                .endpoint(metaData.getPath());
        
        // 解析元数据中的参数信息
        List<McpToolParameter> parameters = extractParameters(metaData);
        builder.parameters(parameters);
        
        // 确定HTTP方法
        HttpMethod httpMethod = determineHttpMethod(metaData);
        builder.httpMethod(httpMethod.name());
        
        // 设置返回类型信息
        builder.returnContentType(MediaType.APPLICATION_JSON_VALUE);
        
        return builder.build();
    }
    
    /**
     * 从元数据中提取参数信息.
     *
     * @param metaData 元数据
     * @return 参数列表
     */
    private List<McpToolParameter> extractParameters(final MetaData metaData) {
        List<McpToolParameter> parameters = new ArrayList<>();
        
        // 尝试从元数据的rpcExt中解析参数信息
        try {
            if (Objects.nonNull(metaData.getRpcExt())) {
                Map<String, Object> rpcExtMap = GsonUtils.getInstance().toObjectMap(metaData.getRpcExt());
                if (rpcExtMap.containsKey("parameterTypes")) {
                    String paramTypes = rpcExtMap.get("parameterTypes").toString();
                    String[] paramNames = rpcExtMap.containsKey("paramNames")
                            ? rpcExtMap.get("paramNames").toString().split(",")
                            : new String[0];
                    
                    String[] types = paramTypes.split(",");
                    for (int i = 0; i < types.length; i++) {
                        String name = i < paramNames.length ? paramNames[i] : "param" + (i + 1);
                        String type = mapJavaTypeToMcpType(types[i]);
                        
                        parameters.add(McpToolParameter.builder()
                                .name(name)
                                .type(type)
                                .required(true)
                                .description("Parameter " + name + " of type " + type)
                                .build());
                    }
                }
            }
        } catch (Exception e) {
            LOG.warn("Error extracting parameters from metadata: {}", metaData.getPath(), e);
        }
        
        return parameters;
    }
    
    /**
     * 确定HTTP方法.
     *
     * @param metaData 元数据
     * @return HTTP方法
     */
    private HttpMethod determineHttpMethod(final MetaData metaData) {
        // 基于方法名猜测HTTP方法
        String methodName = metaData.getMethodName().toLowerCase();
        if (methodName.startsWith("get")
                || methodName.startsWith("query")
                || methodName.startsWith("find")) {
            return HttpMethod.GET;
        } else if (methodName.startsWith("create")
                || methodName.startsWith("add")
                || methodName.startsWith("save")) {
            return HttpMethod.POST;
        } else if (methodName.startsWith("update")
                || methodName.startsWith("modify")) {
            return HttpMethod.PUT;
        } else if (methodName.startsWith("delete")
                || methodName.startsWith("remove")) {
            return HttpMethod.DELETE;
        }
        
        // 默认使用POST
        return HttpMethod.POST;
    }
    
    /**
     * 将Java类型映射到MCP类型.
     *
     * @param javaType Java类型
     * @return MCP类型
     */
    private String mapJavaTypeToMcpType(final String javaType) {
        String javaTypeTmp = javaType.trim();
        if (javaTypeTmp.contains("String")
                || javaTypeTmp.contains("CharSequence")) {
            return "string";
        } else if (javaTypeTmp.contains("Integer")
                || javaTypeTmp.contains("Long")
                || javaTypeTmp.contains("Double")
                || javaTypeTmp.contains("Float")
                || javaTypeTmp.contains("Number")) {
            return "number";
        } else if (javaTypeTmp.contains("Boolean")) {
            return "boolean";
        } else {
            return "object";
        }
    }
    
    /**
     * 生成工具名称.
     *
     * @param metaData 元数据
     * @return 工具名称
     */
    private String generateToolName(final MetaData metaData) {
        // 使用服务名和方法名组合
        return metaData.getServiceName() + "_" + metaData.getMethodName();
    }
    
}
