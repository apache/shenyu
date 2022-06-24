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
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.motan.common.annotation.ShenyuMotanClient;
import org.apache.shenyu.client.motan.common.dto.MotanRpcExt;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * Motan Service Event Listener.
 */
public class MotanServiceEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final String BASE_SERVICE_CONFIG = "baseServiceConfig";

    private static final String PATH_SEPARATOR = "/";

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private ApplicationContext applicationContext;

    private final String contextPath;

    private final String appName;

    private final String host;

    private final String port;

    private String group;
    
    public MotanServiceEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        String appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        if (StringUtils.isEmpty(contextPath)) {
            throw new ShenyuClientIllegalArgumentException("motan client must config the contextPath");
        }
        this.contextPath = contextPath;
        this.appName = appName;
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        this.port = props.getProperty(ShenyuClientConstants.PORT);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) throws BeansException {
        applicationContext = contextRefreshedEvent.getApplicationContext();
        Map<String, Object> beans = applicationContext.getBeansWithAnnotation(ShenyuMotanClient.class);
        for (Map.Entry<String, Object> entry : beans.entrySet()) {
            handler(entry.getValue());
        }
    }

    private void handler(final Object bean) {
        if (group == null) {
            group = ((BasicServiceConfigBean) applicationContext.getBean(BASE_SERVICE_CONFIG)).getGroup();
        }
        Integer timeout = Optional.ofNullable(((BasicServiceConfigBean) applicationContext.getBean(BASE_SERVICE_CONFIG)).getRequestTimeout()).orElse(1000);
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        String superPath = buildApiSuperPath(clazz);
        MotanService service = clazz.getAnnotation(MotanService.class);
        ShenyuMotanClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuMotanClient.class);
        if (superPath.contains("*") && Objects.nonNull(beanShenyuClient)) {
            Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method method : methods) {
                publisher.publishEvent(buildMetaDataDTO(clazz, service, beanShenyuClient, method,
                        buildRpcExt(method, timeout), superPath));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuMotanClient shenyuMotanClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuMotanClient.class);
            if (Objects.nonNull(shenyuMotanClient)) {
                publisher.publishEvent(buildMetaDataDTO(clazz, service,
                        shenyuMotanClient, method, buildRpcExt(method, timeout), superPath));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final Class<?> clazz, final MotanService service,
                                                 final ShenyuMotanClient shenyuMotanClient,
                                                 final Method method, final String rpcExt, final String superPath) {
        String appName = this.appName;
        String path = superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName()) : pathJoin(contextPath, superPath, shenyuMotanClient.path());
        String desc = shenyuMotanClient.desc();
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        int port = StringUtils.isBlank(this.port) ? -1 : Integer.parseInt(this.port);
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
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(this.contextPath)
                .path(path)
                .port(port)
                .host(host)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.MOTAN.getName())
                .rpcExt(rpcExt)
                .enabled(shenyuMotanClient.enabled())
                .build();
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

    private String buildApiSuperPath(@NonNull final Class<?> clazz) {
        ShenyuMotanClient shenyuMotanClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuMotanClient.class);
        if (Objects.nonNull(shenyuMotanClient) && StringUtils.isNotBlank(shenyuMotanClient.path())) {
            return shenyuMotanClient.path();
        }
        return "";
    }

    private String pathJoin(@NonNull final String... path) {
        StringBuilder result = new StringBuilder(PATH_SEPARATOR);
        for (String p : path) {
            if (!result.toString().endsWith(PATH_SEPARATOR)) {
                result.append(PATH_SEPARATOR);
            }
            result.append(p.startsWith(PATH_SEPARATOR) ? p.replaceFirst(PATH_SEPARATOR, "") : p);
        }
        return result.toString();
    }

}
