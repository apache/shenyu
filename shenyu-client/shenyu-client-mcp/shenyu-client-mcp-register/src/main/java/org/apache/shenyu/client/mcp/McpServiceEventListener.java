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
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.utils.RequestMethodUtils;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpToolParam;
import org.apache.shenyu.client.mcp.common.eunm.McpParameterType;
import org.apache.shenyu.client.mcp.generator.McpOpenApiGenerator;
import org.apache.shenyu.client.mcp.generator.McpToolsRegisterDTOGenerator;
import org.apache.shenyu.client.mcp.utils.OpenApiConvertorUtil;
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
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
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
public class McpServiceEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuMcpTool> {

    private static final Logger log = LoggerFactory.getLogger(McpServiceEventListener.class);

    private final Environment env;

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     * @param env                            the spring environment
     */
    public McpServiceEventListener(final ShenyuClientConfig clientConfig,
                                   final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                   final Environment env) {
        super(clientConfig, shenyuClientRegisterRepository);
        this.env = env;
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

                    return targetClass.isAnnotationPresent(ShenyuMcpTool.class);
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
    protected void handleClass(final Class<?> clazz, final Object bean, final ShenyuMcpTool beanShenyuClient, final String superPath) {
    }

    @Override
    protected void handleMethod(final Object bean, final Class<?> clazz, final ShenyuMcpTool classMcpClient, final Method method, final String superPath) {
        ShenyuMcpTool methodMcpClient;
        if (method.isAnnotationPresent(ShenyuMcpTool.class)) {
            methodMcpClient = method.getAnnotation(ShenyuMcpTool.class);
        } else {
            return;
        }

        Operation operation = OpenApiConvertorUtil.convertOperation(methodMcpClient.operation());

        List<io.swagger.v3.oas.models.parameters.Parameter> parameters;
        String methodType = methodMcpClient.operation().method();

        if (ArrayUtils.isNotEmpty(methodMcpClient.operation().parameters())) {
            parameters = Arrays.stream(methodMcpClient.operation().parameters())
                    .map(OpenApiConvertorUtil::convertParameter)
                    .collect(Collectors.toCollection(ArrayList::new));
        } else {
            parameters = new ArrayList<>();
        }

        // inject default ToolDescription without shenyuMcpTool operation description configuration
        String toolDescription = injectToolDescription(operation.getDescription(), method);

        // inject default MethodType without ShenyuMcpTool operation method configuration
        methodType = injectMethodType(methodType, method);

        // inject default Parameters without ShenyuMcpTool operation parameters configuration
        injectParameter(parameters, method);

        operation.setDescription(toolDescription);
        operation.setParameters(parameters);

        org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool shenyuMcpToolMethod = new org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool(methodMcpClient);

        // inject default ToolDescription without shenyuMcpTool operation description configuration
        String toolName = injectToolName(methodMcpClient.toolName(), method);

        shenyuMcpToolMethod.setOperation(operation);
        shenyuMcpToolMethod.setMethod(methodType);
        shenyuMcpToolMethod.setToolName(toolName);

        List<String> namespaceIds = this.getNamespace();
        List<String> mergeUrls = findMergeUrl(clazz, method);
        mergeUrls.forEach(url -> {
            namespaceIds.forEach(namespaceId -> getPublisher().publishEvent(
                    buildMcpToolsRegisterDTO(bean, clazz, classMcpClient, shenyuMcpToolMethod,
                            superPath, method, url, namespaceId)));
        });
    }

    private String injectToolName(final String toolName, final Method method) {
        if (StringUtils.isNoneBlank(toolName)) {
            return toolName;
        }
        return method.getName();
    }

    private String injectToolDescription(final String description, final Method method) {
        if (StringUtils.isNoneBlank(description)) {
            return description;
        }
        return method.getName();
    }

    private String injectMethodType(final String methodType, final Method method) {
        if (StringUtils.isNoneBlank(methodType)) {
            return methodType;
        }
        List<String> requestMethodTypes = RequestMethodUtils.getRequestMethodTypes(method);
        if (requestMethodTypes.size() != 1) {
            log.warn("Method [{}] in class [{}] has no restful mapping annotation; defaulting to GET",
                    method.getName(), method.getDeclaringClass().getName());
            return "GET";
        } else {
            return requestMethodTypes.get(0);
        }
    }

    private void injectParameter(final List<io.swagger.v3.oas.models.parameters.Parameter> parametersList, final Method method) {
        if (!parametersList.isEmpty()) {
            return;
        }
        List<String> parameterPositions = RequestMethodUtils.getParameterPositions(method);
        String[] parameterNames = RequestMethodUtils.getParameterNames(method);
        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            if (parameters[i].isAnnotationPresent(ShenyuMcpToolParam.class)) {
                ShenyuMcpToolParam mcpToolParam = parameters[i].getAnnotation(ShenyuMcpToolParam.class);
                if (Objects.nonNull(mcpToolParam.parameter())) {
                    io.swagger.v3.oas.models.parameters.Parameter parameterObject = OpenApiConvertorUtil.convertParameter(mcpToolParam.parameter());
                    // inject in
                    if (StringUtils.isBlank(parameterObject.getIn())) {
                        parameterObject.setIn(parameterPositions.get(i));
                    }
                    // inject required
                    if (Objects.isNull(parameterObject.getRequired())) {
                        parameterObject.setRequired(false);
                    }
                    // inject name
                    if (StringUtils.isBlank(parameterObject.getName())) {
                        parameterObject.setName(parameterNames[i]);
                    }
                    // inject description
                    if (StringUtils.isBlank(parameterObject.getDescription())) {
                        parameterObject.setDescription(parameterNames[i]);
                    }
                    // inject type
                    if (Objects.isNull(parameterObject.getSchema()) || StringUtils.isBlank(parameterObject.getSchema().getType())) {
                        Schema<Object> schema = new Schema<>();
                        McpParameterType parameterType = McpParameterType.fromParameter(parameters[i]);
                        schema.setType(parameterType.getTypeName());

                        parameterObject.setSchema(schema);
                    }
                    parametersList.add(parameterObject);
                }

            }
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
        final String servletPath = StringUtils.defaultString(this.env.getProperty("spring.mvc.servlet.path"), "");
        final String servletContextPath = StringUtils.defaultString(this.env.getProperty("server.servlet.context-path"), "");
        final String rootPath = concatPaths(servletContextPath, servletPath);
        for (String cp : classPaths) {
            for (String mp : methodPaths) {
                String path = concatPaths(cp, mp);
                String prefix = concatPaths(getContextPath(), rootPath);
                String finalPath = concatPaths(prefix, path);
                combinedPaths.add(finalPath);
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

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuMcpTool beanShenyuClient) {
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
    protected Class<ShenyuMcpTool> getAnnotationType() {
        return ShenyuMcpTool.class;
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuMcpTool methodShenyuClient) {
        return null;
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean, final ShenyuMcpTool shenyuClient, final String path,
                                                   final Class<?> clazz, final Method method, final String namespaceId) {
        ShenyuMcpTool methodClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuMcpTool.class);
        String desc = shenyuClient.desc();
        String configRuleName = null;
        if (Objects.nonNull(methodClient)) {
            configRuleName = injectToolName(methodClient.toolName(), method);
        }
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
                                                         final ShenyuMcpTool classShenyuClient,
                                                         final org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool shenyuMcpTool,
                                                         final String superPath, final Method method,
                                                         final String url, final String namespaceId) {
        validateClientConfig(shenyuMcpTool, url);
        JsonObject openApiJson = McpOpenApiGenerator.generateOpenApiJson(classShenyuClient, shenyuMcpTool, url);
        McpToolsRegisterDTO mcpToolsRegisterDTO = McpToolsRegisterDTOGenerator.generateRegisterDTO(shenyuMcpTool, openApiJson, url, namespaceId);
        MetaDataRegisterDTO metaDataRegisterDTO = buildMetaDataDTO(bean, classShenyuClient, superPath, clazz, method, namespaceId);
        metaDataRegisterDTO.setEnabled(shenyuMcpTool.getEnable());
        mcpToolsRegisterDTO.setMetaDataRegisterDTO(metaDataRegisterDTO);
        return mcpToolsRegisterDTO;
    }

    private void validateClientConfig(final org.apache.shenyu.client.mcp.common.dto.ShenyuMcpTool methodShenyuClient, final String url) {
        if (StringUtils.isBlank(url)) {
            log.error("OpenAPI pathKey is null or empty, please check OpenApiConfig");
            throw new IllegalArgumentException("OpenAPI pathKey cannot be null or empty");
        }

        if (StringUtils.isBlank(methodShenyuClient.getMethod())) {
            log.error("OpenAPI methodType is null or empty, please check OpenApiConfig");
            throw new IllegalArgumentException("OpenAPI methodType cannot be null or empty");
        }
    }

}
