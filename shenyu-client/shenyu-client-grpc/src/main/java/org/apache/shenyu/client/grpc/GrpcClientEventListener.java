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

package org.apache.shenyu.client.grpc;

import com.google.common.collect.Lists;
import io.grpc.BindableService;
import io.grpc.MethodDescriptor;
import io.grpc.ServerServiceDefinition;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.apache.shenyu.client.grpc.common.dto.GrpcExt;
import org.apache.shenyu.client.grpc.json.JsonServerServiceInterceptor;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.javatuples.Sextet;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Shenyu grpc client event listener.
 */
public class GrpcClientEventListener extends AbstractContextRefreshedEventListener<BindableService, ShenyuGrpcClient> {
    
    private final List<ServerServiceDefinition> serviceDefinitions = Lists.newArrayList();
    
    /**
     * Instantiates a new Shenyu client bean post processor.
     *
     * @param clientConfig the shenyu grpc client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public GrpcClientEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
        if (StringUtils.isAnyBlank(getContextPath(), getIpAndPort(), getPort())) {
            throw new ShenyuClientIllegalArgumentException("grpc client must config the contextPath, ipAndPort");
        }
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, BindableService> beans) {
        ShenyuGrpcClient shenyuGrpcClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuGrpcClient.class);
        if (Objects.isNull(shenyuGrpcClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuGrpcClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.GRPC, version);
    }

    @Override
    protected void handle(final String beanName, final BindableService bean) {
        exportJsonGenericService(bean);
        super.handle(beanName, bean);
    }
    
    @Override
    protected Map<String, BindableService> getBeans(final ApplicationContext context) {
        return context.getBeansOfType(BindableService.class);
    }
    
    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, BindableService> beans) {
        return URIRegisterDTO.builder()
                .contextPath(getContextPath())
                .appName(getIpAndPort())
                .rpcType(RpcTypeEnum.GRPC.getName())
                .host(buildHost())
                .port(Integer.parseInt(getPort()))
                .build();
    }
    
    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuGrpcClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && !StringUtils.isBlank(beanShenyuClient.path())) {
            return beanShenyuClient.path();
        }
        return "";
    }
    
    @Override
    protected Class<ShenyuGrpcClient> getAnnotationType() {
        return ShenyuGrpcClient.class;
    }
    
    @Override
    protected String buildApiPath(final Method method, final String superPath, @NonNull final ShenyuGrpcClient methodShenyuClient) {
        final String contextPath = getContextPath();
        return superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName())
                : pathJoin(contextPath, superPath, methodShenyuClient.path());
    }
    
    @Override
    protected void handleClass(final Class<?> clazz, final BindableService bean, @NonNull final ShenyuGrpcClient beanShenyuClient, final String superPath) {
        Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
        for (Method method : methods) {
            if (Modifier.isPublic(method.getModifiers())) {
                getPublisher().publishEvent(buildMetaDataDTO(bean, beanShenyuClient, buildApiPath(method, superPath, beanShenyuClient), clazz, method));
            }
        }
    }
    
    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final BindableService bean, @NonNull final ShenyuGrpcClient shenyuClient, final String path, final Class<?> clazz, final Method method) {
        String desc = shenyuClient.desc();
        String configRuleName = shenyuClient.ruleName();
        String ruleName = StringUtils.defaultIfBlank(configRuleName, path);
        String methodName = method.getName();
        String packageName = buildPackageName(clazz);
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MethodDescriptor.MethodType methodType = JsonServerServiceInterceptor.getMethodTypeMap().get(packageName + "/" + methodName);
        return MetaDataRegisterDTO.builder()
                .appName(getIpAndPort())
                .serviceName(packageName)
                .methodName(methodName)
                .contextPath(getContextPath())
                .host(buildHost())
                .port(Integer.parseInt(getPort()))
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.GRPC.getName())
                .rpcExt(buildRpcExt(shenyuClient, methodType))
                .enabled(shenyuClient.enabled())
                .build();
    }
    
    private String buildHost() {
        final String host = this.getHost();
        return IpUtils.isCompleteHost(host) ? host : IpUtils.getHost(host);
    }
    
    private String buildPackageName(final Class<?> clazz) {
        Class<?> parent = clazz.getSuperclass();
        Class<?> classes = parent.getDeclaringClass();
        String packageName = null;
        try {
            String serviceName = ShenyuClientConstants.SERVICE_NAME;
            Field field = classes.getField(serviceName);
            field.setAccessible(true);
            packageName = field.get(null).toString();
        } catch (Exception e) {
            LOG.error(String.format("SERVICE_NAME field not found: %s", classes), e);
        }
        return packageName;
    }
    
    private String buildRpcExt(final ShenyuGrpcClient shenyuGrpcClient,
                               final MethodDescriptor.MethodType methodType) {
        GrpcExt build = GrpcExt.builder()
                .timeout(shenyuGrpcClient.timeout())
                .methodType(methodType)
                .build();
        return GsonUtils.getInstance().toJson(build);
    }

    private void exportJsonGenericService(final BindableService bindableService) {
        ServerServiceDefinition serviceDefinition = bindableService.bindService();
        
        try {
            ServerServiceDefinition jsonDefinition = JsonServerServiceInterceptor.useJsonMessages(serviceDefinition);
            serviceDefinitions.add(serviceDefinition);
            serviceDefinitions.add(jsonDefinition);
        } catch (Exception e) {
            LOG.error("export json generic service is fail", e);
        }
    }

    /**
     * get serviceDefinitions.
     *
     * @return serviceDefinitions
     */
    public List<ServerServiceDefinition> getServiceDefinitions() {
        return serviceDefinitions;
    }
}
