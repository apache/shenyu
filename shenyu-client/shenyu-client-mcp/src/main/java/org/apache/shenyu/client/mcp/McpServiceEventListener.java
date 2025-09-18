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
import io.swagger.v3.oas.annotations.servers.Server;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Mcp service event Listener.
 */
public class McpServiceEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuMcpClient> {

    private static final Logger log = LoggerFactory.getLogger(McpServiceEventListener.class);

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public McpServiceEventListener(final ShenyuClientConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        return null;
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
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
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, Object> beans, final String namespaceId) {
        return null;
    }

    @Override
    protected String getClientName() {
        return RpcTypeEnum.MCP.getName();
    }

    @Override
    protected void handleClass(final Class<?> clazz, final Object bean, final ShenyuMcpClient beanShenyuClient, final String superPath) {
    }

    @Override
    protected void handleMethod(final Object bean, final Class<?> clazz, final ShenyuMcpClient beanShenyuClient, final Method method, final String superPath) {
        List<String> namespaceIds = this.getNamespace();
        ShenyuMcpClient classMcpClient;
        if (clazz.isAnnotationPresent(ShenyuMcpClient.class)) {
            classMcpClient = clazz.getAnnotation(ShenyuMcpClient.class);
        } else {
            return;
        }
        ShenyuMcpClient methodMcpClient;
        if (method.isAnnotationPresent(ShenyuMcpClient.class)) {
            methodMcpClient = method.getAnnotation(ShenyuMcpClient.class);
        } else {
            return;
        }

        List<String> mergeUrls = findMergeUrl(clazz, method);
        mergeUrls.forEach(url -> {
            namespaceIds.forEach(namespaceId -> getPublisher().publishEvent(buildMcpToolsRegisterDTO(bean, clazz, classMcpClient, methodMcpClient, superPath, method, url, namespaceId)));
        });
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuMcpClient beanShenyuClient) {
        Server[] servers = beanShenyuClient.definition().servers();
        if (servers.length != 1) {
            log.warn("The shenyuMcp service supports only a single server entry. Please ensure that only one server is configured");
        }
        String superUrl = servers[0].url();
        if (StringUtils.isNotEmpty(superUrl)) {
            return superUrl;
        }
        return "";
    }

    @Override
    protected Class<ShenyuMcpClient> getAnnotationType() {
        return ShenyuMcpClient.class;
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuMcpClient methodShenyuClient) {
        return null;
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean, final ShenyuMcpClient shenyuClient, final String path, final Class<?> clazz, final Method method, final String namespaceId) {
        String desc = shenyuClient.desc();
        String configRuleName = shenyuClient.toolName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        String serviceName = shenyuClient.definition().info().title();
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
                .namespaceId(namespaceId)
                .build();
    }

    private McpToolsRegisterDTO buildMcpToolsRegisterDTO(final Object bean, final Class<?> clazz,
                                                         final ShenyuMcpClient classShenyuClient,
                                                         final ShenyuMcpClient methodShenyuClient, final String superPath,
                                                         final Method method, final String url, final String namespaceId) {
        validateClientConfig(methodShenyuClient, url);
        JsonObject openApiJson = McpOpenApiGenerator.generateOpenApiJson(classShenyuClient, methodShenyuClient, url);
        McpToolsRegisterDTO mcpToolsRegisterDTO = McpToolsRegisterDTOGenerator.generateRegisterDTO(classShenyuClient, methodShenyuClient, openApiJson, url, namespaceId);
        MetaDataRegisterDTO metaDataRegisterDTO = buildMetaDataDTO(bean, classShenyuClient, superPath, clazz, method, namespaceId);
        metaDataRegisterDTO.setEnabled(methodShenyuClient.enabled());
        mcpToolsRegisterDTO.setMetaDataRegisterDTO(metaDataRegisterDTO);
        return mcpToolsRegisterDTO;
    }

    private void validateClientConfig(final ShenyuMcpClient beanShenyuClient, final String url) {
        if (StringUtils.isBlank(url)) {
            log.error("OpenAPI pathKey is null or empty, please check OpenApiConfig");
            throw new IllegalArgumentException("OpenAPI pathKey cannot be null or empty");
        }

        if (StringUtils.isBlank(beanShenyuClient.operation().method())) {
            log.error("OpenAPI methodType is null or empty, please check OpenApiConfig");
            throw new IllegalArgumentException("OpenAPI methodType cannot be null or empty");
        }
    }

    private List<String> findMergeUrl(final Class<?> clazz, final Method method) {
        List<String> classPaths = Collections.emptyList();
        RequestMapping classMapping = AnnotatedElementUtils.findMergedAnnotation(clazz, RequestMapping.class);
        if (Objects.nonNull(classMapping)) {
            String[] paths = classMapping.path();
            if (paths.length > 0) {
                classPaths = Arrays.asList(paths);
            } else {
                String[] values = classMapping.value();
                if (values.length > 0) {
                    classPaths = Arrays.asList(values);
                }
            }
        }
        List<String> methodPaths = Collections.emptyList();
        RequestMapping methodMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
        if (Objects.nonNull(methodMapping)) {
            String[] paths = methodMapping.path();
            if (paths.length > 0) {
                methodPaths = Arrays.asList(paths);
            } else {
                String[] values = methodMapping.value();
                if (values.length > 0) {
                    methodPaths = Arrays.asList(values);
                }
            }
        }
        if (classPaths.isEmpty()) {
            classPaths = Collections.singletonList("");
        }
        if (methodPaths.isEmpty()) {
            methodPaths = Collections.singletonList("");
        }

        List<String> combinedPaths = new ArrayList<>();
        for (String cp : classPaths) {
            for (String mp : methodPaths) {
                String path = concatPaths(cp, mp);
                combinedPaths.add(path);
            }
        }
        return combinedPaths;
    }

    private static String concatPaths(final String path1, final String path2) {
        if (path1.endsWith("/") && path2.startsWith("/")) {
            return path1 + path2.substring(1);
        } else if (!path1.endsWith("/") && !path2.startsWith("/")) {
            if (path1.isEmpty() || path2.isEmpty()) {
                return path1 + path2;
            }
            return path1 + "/" + path2;
        } else {
            return path1 + path2;
        }
    }

}
