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

package org.apache.shenyu.client.mcp;

import com.google.gson.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpClient;
import org.apache.shenyu.client.mcp.generator.McpOpenApiGenerator;
import org.apache.shenyu.client.mcp.generator.McpToolsRegisterDTOGenerator;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.javatuples.Sextet;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.stereotype.Controller;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Mcp service event Listener.
 */
public class McpServiceEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuMcpClient> {

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public McpServiceEventListener(ShenyuClientConfig clientConfig, ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(Method method, Annotation annotation, Map<String, Object> beans) {
        return null;
    }

    @Override
    protected Map<String, Object> getBeans(ApplicationContext context) {
        Map<String, Object> controllerBeans = context.getBeansWithAnnotation(Controller.class);
        return controllerBeans.entrySet().stream()
                .filter(entry -> {
                    Object bean = entry.getValue();

                    Class<?> targetClass = AopUtils.getTargetClass(bean);

                    return targetClass.isAnnotationPresent(ShenyuMcpClient.class);
                })
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(ApplicationContext context, Map<String, Object> beans, String namespaceId) {
        return null;
    }

    @Override
    protected String getClientName() {
        return RpcTypeEnum.MCP.getName();
    }

    @Override
    protected void handleClass(Class<?> clazz, Object bean, ShenyuMcpClient beanShenyuClient, String superPath) {
    }

    @Override
    protected void handleMethod(Object bean, Class<?> clazz, ShenyuMcpClient beanShenyuClient, Method method, String superPath) {
        List<String> namespaceIds = this.getNamespace();
        ShenyuMcpClient methodMcpClient;
        if (method.isAnnotationPresent(ShenyuMcpClient.class)) {
            methodMcpClient = method.getAnnotation(ShenyuMcpClient.class);
        }else {
            return;
        }
        namespaceIds.forEach(namespaceId -> getPublisher().publishEvent(buildMcpToolsRegisterDTO(bean, clazz, methodMcpClient, superPath, method, namespaceId)));
    }

    @Override
    protected String buildApiSuperPath(Class<?> clazz, ShenyuMcpClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && !StringUtils.isBlank(beanShenyuClient.openApi().path().path())) {
            return beanShenyuClient.openApi().path().path();
        }
        return "";
    }

    @Override
    protected Class<ShenyuMcpClient> getAnnotationType() {
        return ShenyuMcpClient.class;
    }

    @Override
    protected String buildApiPath(Method method, String superPath, ShenyuMcpClient methodShenyuClient) {
        return null;
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(Object bean, ShenyuMcpClient shenyuClient, String path, Class<?> clazz, Method method, String namespaceId) {
        String desc = shenyuClient.desc();
        String configRuleName = shenyuClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        String serviceName = shenyuClient.openApi().info().title();
        return MetaDataRegisterDTO.builder()
                .appName(this.getAppName())
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(this.getContextPath())
                .path(path)
                .port(Integer.parseInt(super.getPort()))
                .host(super.getHost())
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.MCP.getName())
                .enabled(shenyuClient.enabled())
                .namespaceId(namespaceId)
                .build();
    }

    private McpToolsRegisterDTO buildMcpToolsRegisterDTO(Object bean, final Class<?> clazz, final ShenyuMcpClient beanShenyuClient, final String superPath, Method method, final String namespaceId) {
        JsonObject openApiJson = McpOpenApiGenerator.generateOpenApiJson(beanShenyuClient);
        McpToolsRegisterDTO mcpToolsRegisterDTO = McpToolsRegisterDTOGenerator.generateRegisterDTO(beanShenyuClient, openApiJson, namespaceId);
        mcpToolsRegisterDTO.setMetaDataRegisterDTO(buildMetaDataDTO(bean, beanShenyuClient, superPath, clazz, method, namespaceId));
        return mcpToolsRegisterDTO;
    }


}
