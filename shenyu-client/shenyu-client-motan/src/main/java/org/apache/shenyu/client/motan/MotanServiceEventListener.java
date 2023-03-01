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

package org.apache.shenyu.client.motan;

import com.weibo.api.motan.config.springsupport.BasicServiceConfigBean;
import com.weibo.api.motan.config.springsupport.annotation.MotanService;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.motan.common.annotation.ShenyuMotanClient;
import org.apache.shenyu.client.motan.common.dto.MotanRpcExt;
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
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Motan Service Event Listener.
 */
public class MotanServiceEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuMotanClient> {

    private static final String BASE_SERVICE_CONFIG = "baseServiceConfig";

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private ApplicationContext applicationContext;

    private String group;

    public MotanServiceEventListener(final PropertiesConfig clientConfig,
                                     final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        ShenyuMotanClient shenyuMotanClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuMotanClient.class);
        if (Objects.isNull(shenyuMotanClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuMotanClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.MOTAN, version);
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
        applicationContext = context;
        group = ((BasicServiceConfigBean) applicationContext.getBean(BASE_SERVICE_CONFIG)).getGroup();
        return context.getBeansWithAnnotation(ShenyuMotanClient.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, Object> beans) {
        return URIRegisterDTO.builder()
                .contextPath(this.getContextPath())
                .appName(this.getAppName())
                .rpcType(RpcTypeEnum.MOTAN.getName())
                .host(this.getHost())
                .port(Integer.parseInt(this.getPort()))
                .build();
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuMotanClient shenyuMotanClient) {
        if (Objects.nonNull(shenyuMotanClient) && !StringUtils.isBlank(shenyuMotanClient.path())) {
            return shenyuMotanClient.path();
        }
        return "";
    }

    @Override
    protected Class<ShenyuMotanClient> getAnnotationType() {
        return ShenyuMotanClient.class;
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean, final ShenyuMotanClient shenyuMotanClient, final String superPath, final Class<?> clazz, final Method method) {
        Integer timeout = Optional.ofNullable(((BasicServiceConfigBean) applicationContext.getBean(BASE_SERVICE_CONFIG)).getRequestTimeout()).orElse(1000);
        MotanService service = AnnotatedElementUtils.findMergedAnnotation(clazz, MotanService.class);
        String path = superPath.contains("*") ? pathJoin(getContextPath(), superPath.replace("*", ""), method.getName()) : pathJoin(getContextPath(), superPath, shenyuMotanClient.path());
        String desc = shenyuMotanClient.desc();
        String host = IpUtils.isCompleteHost(this.getHost()) ? this.getHost() : IpUtils.getHost(this.getHost());
        int port = StringUtils.isBlank(this.getPort()) ? -1 : Integer.parseInt(this.getPort());
        String configRuleName = shenyuMotanClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        String serviceName;
        if (void.class.equals(service.interfaceClass())) {
            if (clazz.getInterfaces().length > 0) {
                serviceName = clazz.getInterfaces()[0].getName();
            } else {
                throw new ShenyuClientIllegalArgumentException("Failed to export remote service class " + clazz.getName()
                        + ", cause: The @Service undefined interfaceClass or interfaceName, and the service class unimplemented any interfaces.");
            }
        } else {
            serviceName = service.interfaceClass().getName();
        }
        return MetaDataRegisterDTO.builder()
                .appName(this.getAppName())
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(this.getContextPath())
                .path(path)
                .port(port)
                .host(host)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.MOTAN.getName())
                .rpcExt(buildRpcExt(method, timeout))
                .enabled(shenyuMotanClient.enabled())
                .build();
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuMotanClient methodShenyuClient) {
        return superPath.contains("*")
                ? pathJoin(this.getContextPath(), superPath.replace("*", ""), method.getName())
                : pathJoin(this.getContextPath(), superPath, methodShenyuClient.path());
    }

    @Override
    protected void handleClass(final Class<?> clazz, final Object bean, final ShenyuMotanClient beanShenyuClient, final String superPath) {
        Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
        for (Method method : methods) {
            publisher.publishEvent(buildMetaDataDTO(bean, beanShenyuClient, superPath, clazz, method));
        }
    }

    private MotanRpcExt.RpcExt buildRpcExt(final Method method) {
        String[] paramNames = localVariableTableParameterNameDiscoverer.getParameterNames(method);
        List<Pair<String, String>> params = new ArrayList<>();
        if (paramNames != null && paramNames.length > 0) {
            Class<?>[] paramTypes = method.getParameterTypes();
            for (int i = 0; i < paramNames.length; i++) {
                params.add(Pair.of(paramTypes[i].getName(), paramNames[i]));
            }
        }
        return new MotanRpcExt.RpcExt(method.getName(), params);
    }

    private String buildRpcExt(final Method method, final Integer timeout) {
        List<MotanRpcExt.RpcExt> list = new ArrayList<>();
        list.add(buildRpcExt(method));
        MotanRpcExt buildList = new MotanRpcExt(list, group, timeout);
        return GsonUtils.getInstance().toJson(buildList);
    }
}
