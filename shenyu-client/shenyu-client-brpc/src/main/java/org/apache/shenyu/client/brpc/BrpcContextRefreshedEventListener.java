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

package org.apache.shenyu.client.brpc;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcClient;
import org.apache.shenyu.client.brpc.common.dto.BrpcRpcExt;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
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
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type Brpc context refreshed event listener.
 */
public class BrpcContextRefreshedEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuBrpcClient> {

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    /**
     * Instantiates a new Brpc context refreshed event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public BrpcContextRefreshedEventListener(final PropertiesConfig clientConfig,
                                             final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        ShenyuBrpcClient shenyuBrpcClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuBrpcClient.class);
        if (Objects.isNull(shenyuBrpcClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuBrpcClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.BRPC, version);
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
        return context.getBeansWithAnnotation(ShenyuBrpcClient.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, Object> beans) {
        return URIRegisterDTO.builder()
                .contextPath(this.getContextPath())
                .appName(this.getAppName())
                .rpcType(RpcTypeEnum.BRPC.getName())
                .host(this.getHost())
                .port(Integer.parseInt(this.getPort()))
                .build();
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuBrpcClient shenyuBrpcClient) {
        if (Objects.nonNull(shenyuBrpcClient) && !StringUtils.isBlank(shenyuBrpcClient.path())) {
            return shenyuBrpcClient.path();
        }
        return "";
    }

    @Override
    protected Class<ShenyuBrpcClient> getAnnotationType() {
        return ShenyuBrpcClient.class;
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuBrpcClient shenyuBrpcClient) {
        return superPath.contains("*")
                ? pathJoin(this.getContextPath(), superPath.replace("*", ""), method.getName())
                : pathJoin(this.getContextPath(), superPath, shenyuBrpcClient.path());
    }

    @Override
    protected void handleClass(final Class<?> clazz, final Object bean, final ShenyuBrpcClient shenyuBrpcClient, final String superPath) {
        Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
        for (Method method : methods) {
            publisher.publishEvent(buildMetaDataDTO(bean, shenyuBrpcClient, superPath, clazz, method));
        }
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean, final ShenyuBrpcClient shenyuBrpcClient, final String superPath, final Class<?> clazz, final Method method) {
        String serviceName = clazz.getInterfaces().length > 0 ? clazz.getInterfaces()[0].getName() : clazz.getName();
        String path = superPath;
        String desc = shenyuBrpcClient.desc();
        String host = IpUtils.isCompleteHost(this.getHost()) ? this.getHost() : IpUtils.getHost(this.getHost());
        String configRuleName = shenyuBrpcClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        int port = StringUtils.isBlank(this.getPort()) ? -1 : Integer.parseInt(this.getPort());
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(this.getAppName())
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(this.getContextPath())
                .path(path)
                .port(port)
                .host(host)
                .pathDesc(desc)
                .ruleName(ruleName)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.BRPC.getName())
                .rpcExt(buildRpcExt(method, host, port))
                .enabled(shenyuBrpcClient.enabled())
                .build();
    }

    private String buildRpcExt(final Method method, final String host, final int port) {
        List<BrpcRpcExt.RpcExt> list = new ArrayList<>();
        list.add(build(method));
        BrpcRpcExt buildList = new BrpcRpcExt(list, host, port);
        return GsonUtils.getInstance().toJson(buildList);
    }

    private BrpcRpcExt.RpcExt build(final Method method) {
        String[] paramNames = localVariableTableParameterNameDiscoverer.getParameterNames(method);
        List<Pair<String, String>> params = new ArrayList<>();
        if (paramNames != null && paramNames.length > 0) {
            Class<?>[] paramTypes = method.getParameterTypes();
            for (int i = 0; i < paramNames.length; i++) {
                params.add(Pair.of(paramTypes[i].getName(), paramNames[i]));
            }
        }
        return new BrpcRpcExt.RpcExt(method.getName(), params);
    }
}
