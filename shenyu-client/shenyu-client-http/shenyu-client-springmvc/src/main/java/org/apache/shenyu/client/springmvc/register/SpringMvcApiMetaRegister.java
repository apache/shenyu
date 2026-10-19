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

package org.apache.shenyu.client.springmvc.register;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.matcher.AnnotatedApiDefinitionMatcher;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.registrar.AbstractApiMetaRegistrar;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class SpringMvcApiMetaRegister extends AbstractApiMetaRegistrar {

    private final Matcher<ApiBean.ApiDefinition> apiDefinitionMatcher =
            new AnnotatedApiDefinitionMatcher(ShenyuSpringMvcClient.class)
                    .or(api -> AnnotationUtils.isAnnotationDeclaredLocally(ShenyuSpringMvcClient.class, api.getBeanClass()));

    private final Boolean addPrefixed;

    private final String appName;

    private final String host;

    private final Integer port;
    
    private final ClientRegisterConfig clientRegisterConfig;

    public SpringMvcApiMetaRegister(final ShenyuClientRegisterEventPublisher publisher,
                                    final ClientRegisterConfig clientRegisterConfig) {
        super(publisher);

        this.addPrefixed = clientRegisterConfig.getAddPrefixed();
        this.appName = clientRegisterConfig.getAppName();
        this.host = clientRegisterConfig.getHost();
        this.port = clientRegisterConfig.getPort();
        this.clientRegisterConfig = clientRegisterConfig;
    }

    @Override
    protected Boolean preMatch(final ApiBean apiBean) {

        ShenyuSpringMvcClient annotation = apiBean.getAnnotation(ShenyuSpringMvcClient.class);

        return Objects.nonNull(annotation) && isWildcard(annotation);
    }

    @Override
    protected List<MetaDataRegisterDTO> preParseList(final ApiBean apiBean) {
        ShenyuSpringMvcClient annotation = apiBean.getAnnotation(ShenyuSpringMvcClient.class);
        List<String> beanPaths = distinctNonBlank(annotation.path());
        if (beanPaths.isEmpty()) {
            beanPaths = Collections.singletonList(StringUtils.EMPTY);
        }
        List<MetaDataRegisterDTO> result = new ArrayList<>();
        for (String namespaceId : clientRegisterConfig.getNamespace()) {
            Set<String> seen = new LinkedHashSet<>();
            for (String beanPath : beanPaths) {
                String apiPath = PathUtils.pathJoin(clientRegisterConfig.getContextPath(), beanPath);
                if (seen.add(apiPath)) {
                    result.add(buildPreMetaData(apiBean, annotation, apiPath, namespaceId));
                }
            }
        }
        return result;
    }

    @Override
    protected MetaDataRegisterDTO preParse(final ApiBean apiBean) {
        return preParse(apiBean, clientRegisterConfig.getNamespace().get(0));
    }

    private MetaDataRegisterDTO preParse(final ApiBean apiBean, final String namespaceId) {

        ShenyuSpringMvcClient annotation = apiBean.getAnnotation(ShenyuSpringMvcClient.class);
        List<String> beanPaths = distinctNonBlank(annotation.path());
        String apiPath = PathUtils.pathJoin(clientRegisterConfig.getContextPath(),
                beanPaths.isEmpty() ? StringUtils.EMPTY : beanPaths.get(0));

        return buildPreMetaData(apiBean, annotation, apiPath, namespaceId);
    }

    private MetaDataRegisterDTO buildPreMetaData(final ApiBean apiBean, final ShenyuSpringMvcClient annotation,
                                                 final String apiPath, final String namespaceId) {
        return MetaDataRegisterDTO.builder()
                .namespaceId(namespaceId)
                .contextPath(clientRegisterConfig.getContextPath())
                .addPrefixed(addPrefixed)
                .appName(appName)
                .serviceName(apiBean.getBeanClass().getName())
                .host(host)
                .port(port)
                .methodName(null)
                .path(apiPath)
                .pathDesc(annotation.desc())
                .parameterTypes(null)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(annotation.enabled())
                .ruleName(StringUtils.defaultIfBlank(annotation.ruleName(), apiPath))
                .registerMetaData(annotation.registerMetaData())
                .build();
    }

    @Override
    protected Boolean match(final ApiBean apiBean) {
        ShenyuSpringMvcClient annotation = apiBean.getAnnotation(ShenyuSpringMvcClient.class);
        if (Objects.nonNull(annotation)) {
            return !isWildcard(annotation);
        }
        return true;
    }

    @Override
    protected Boolean match(final ApiBean.ApiDefinition apiDefinition) {
        return apiDefinitionMatcher.match(apiDefinition);
    }

    @Override
    protected List<MetaDataRegisterDTO> parse(final ApiBean.ApiDefinition apiDefinition) {

        ShenyuSpringMvcClient methodAnnotation = apiDefinition.getAnnotation(ShenyuSpringMvcClient.class);

        ShenyuSpringMvcClient classAnnotation = apiDefinition.getApiBean()
                .getAnnotation(ShenyuSpringMvcClient.class);

        List<String> beanPaths = resolveBeanPaths(apiDefinition, classAnnotation);
        List<String> methodPaths = resolveMethodPaths(apiDefinition, methodAnnotation);

        String pathDesc = Objects.isNull(methodAnnotation) ? classAnnotation.desc() : methodAnnotation.desc();

        boolean enabled = (Objects.isNull(classAnnotation) || classAnnotation.enabled())
                && (Objects.isNull(methodAnnotation) || methodAnnotation.enabled());

        String explicitRuleName = Objects.isNull(methodAnnotation) ? null : methodAnnotation.ruleName();

        boolean registerMetaData = (Objects.isNull(classAnnotation) || classAnnotation.registerMetaData())
                && (Objects.isNull(methodAnnotation) || methodAnnotation.registerMetaData());

        List<MetaDataRegisterDTO> result = new ArrayList<>();
        for (String namespaceId : clientRegisterConfig.getNamespace()) {
            Set<String> seen = new LinkedHashSet<>();
            for (String beanPath : beanPaths) {
                for (String methodPath : methodPaths) {
                    String apiPath = PathUtils.pathJoin(clientRegisterConfig.getContextPath(), beanPath, methodPath);
                    if (!seen.add(apiPath)) {
                        continue;
                    }
                    String ruleName = StringUtils.isEmpty(explicitRuleName) ? apiPath : explicitRuleName;
                    result.add(MetaDataRegisterDTO.builder()
                            .namespaceId(namespaceId)
                            .contextPath(clientRegisterConfig.getContextPath())
                            .addPrefixed(addPrefixed)
                            .appName(appName)
                            .host(host)
                            .port(port)
                            .serviceName(apiDefinition.getBeanClass().getName())
                            .methodName(apiDefinition.getApiMethodName())
                            .path(apiPath)
                            .pathDesc(pathDesc)
                            .parameterTypes(apiDefinition.getParameterTypes())
                            .rpcType(RpcTypeEnum.HTTP.getName())
                            .enabled(enabled)
                            .ruleName(ruleName)
                            .registerMetaData(registerMetaData)
                            .build());
                }
            }
        }
        return result;
    }

    /**
     * Resolves every effective class-level path alias, honouring an explicit {@link ShenyuSpringMvcClient}
     * override first and falling back to the native {@link RequestMapping} array so that no alias is truncated.
     */
    private List<String> resolveBeanPaths(final ApiBean.ApiDefinition apiDefinition,
                                          final ShenyuSpringMvcClient classAnnotation) {
        if (Objects.nonNull(classAnnotation)) {
            List<String> paths = distinctNonBlank(classAnnotation.path());
            if (!paths.isEmpty()) {
                return paths;
            }
        }
        RequestMapping requestMapping = apiDefinition.getApiBean().getAnnotation(RequestMapping.class);
        if (Objects.nonNull(requestMapping)) {
            List<String> paths = distinctNonBlank(requestMapping.path());
            if (!paths.isEmpty()) {
                return paths;
            }
        }
        return Collections.singletonList(StringUtils.defaultString(apiDefinition.getBeanPath()));
    }

    /**
     * Resolves every effective method-level path alias, honouring an explicit {@link ShenyuSpringMvcClient}
     * override first and falling back to the native {@link RequestMapping} array so that no alias is truncated.
     */
    private List<String> resolveMethodPaths(final ApiBean.ApiDefinition apiDefinition,
                                            final ShenyuSpringMvcClient methodAnnotation) {
        if (Objects.nonNull(methodAnnotation)) {
            List<String> paths = distinctNonBlank(methodAnnotation.path());
            if (!paths.isEmpty()) {
                return paths;
            }
        }
        RequestMapping requestMapping = apiDefinition.getAnnotation(RequestMapping.class);
        if (Objects.nonNull(requestMapping)) {
            List<String> paths = distinctNonBlank(requestMapping.path());
            if (!paths.isEmpty()) {
                return paths;
            }
        }
        return Collections.singletonList(StringUtils.defaultString(apiDefinition.getMethodPath()));
    }

    private boolean isWildcard(final ShenyuSpringMvcClient annotation) {
        return ArrayUtils.isNotEmpty(annotation.path()) && annotation.path()[0].endsWith("/**");
    }

    private List<String> distinctNonBlank(final String[] paths) {
        if (ArrayUtils.isEmpty(paths)) {
            return Collections.emptyList();
        }
        return Arrays.stream(paths)
                .filter(StringUtils::isNotBlank)
                .distinct()
                .collect(Collectors.toList());
    }
}
