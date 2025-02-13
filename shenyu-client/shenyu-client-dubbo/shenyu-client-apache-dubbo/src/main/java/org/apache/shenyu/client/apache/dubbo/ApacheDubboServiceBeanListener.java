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

package org.apache.shenyu.client.apache.dubbo;

import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.client.dubbo.common.dto.DubboRpcExt;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.javatuples.Sextet;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.apache.dubbo.remoting.Constants.DEFAULT_CONNECT_TIMEOUT;

/**
 * The Apache Dubbo ServiceBean Listener.
 */
@SuppressWarnings("all")
public class ApacheDubboServiceBeanListener extends AbstractContextRefreshedEventListener<ServiceBean, ShenyuDubboClient> {

    /**
     * Instantiates a new context refreshed event listener.
     *
     * @param clientConfig                   the shenyu dubbo client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public ApacheDubboServiceBeanListener(final ShenyuClientConfig clientConfig,
                                          final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, ServiceBean> beans) {
        ShenyuDubboClient shenyuDubboClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuDubboClient.class);
        if (Objects.isNull(shenyuDubboClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuDubboClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String defaultVersion = "v0.01";
        Class<?> methodClass = method.getDeclaringClass();
        Class<?>[] interfaces = methodClass.getInterfaces();
        for (Class<?> anInterface : interfaces) {
            if (beans.containsKey(anInterface.getName())) {
                ServiceBean<?> serviceBean = beans.get(anInterface.getName());
                defaultVersion = Optional.ofNullable(serviceBean.getVersion()).orElse(defaultVersion);
            }
        }
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.DUBBO, defaultVersion);
    }

    @Override
    protected Map<String, ServiceBean> getBeans(final ApplicationContext context) {
        return context.getBeansOfType(ServiceBean.class);
    }
    
    @Override
    protected Class<?> getCorrectedClass(final ServiceBean bean) {
        Object refProxy = bean.getRef();
        Class<?> clazz = refProxy.getClass();
        if (AopUtils.isAopProxy(refProxy)) {
            clazz = AopUtils.getTargetClass(refProxy);
        }
        return clazz;
    }
    
    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context,
                                                 final Map<String, ServiceBean> beans,
                                                 final String namespaceId) {
        return beans.entrySet().stream().findFirst().map(entry -> {
            final ServiceBean<?> bean = entry.getValue();
            return URIRegisterDTO.builder()
                    .contextPath(getContextPath())
                    .appName(buildAppName(bean))
                    .rpcType(RpcTypeEnum.DUBBO.getName())
                    .eventType(EventType.REGISTER)
                    .host(super.getHost())
                    .port(Integer.valueOf(getPort()))
                    .namespaceId(namespaceId)
                    .build();
        }).orElse(null);
    }
    
    @Override
    protected String getClientName() {
        return RpcTypeEnum.DUBBO.getName();
    }
    
    private String buildAppName(final ServiceBean<?> serviceBean) {
        String appName = this.getAppName();
        return StringUtils.isBlank(appName) ? serviceBean.getApplication().getName() : appName;
    }
    
    @Override
    protected Class<ShenyuDubboClient> getAnnotationType() {
        return ShenyuDubboClient.class;
    }
    
    @Override
    protected String buildApiSuperPath(final Class<?> clazz,
                                       @Nullable final ShenyuDubboClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && !StringUtils.isBlank(beanShenyuClient.path())) {
            return beanShenyuClient.path();
        }
        return "";
    }

    @Override
    protected void handleClass(final Class<?> clazz,
                               final ServiceBean bean,
                               @NonNull final ShenyuDubboClient beanShenyuClient,
                               final String superPath) {
        List<String> namespaceIds = super.getNamespace();
        Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
        for (String namespaceId : namespaceIds) {
            for (Method method : methods) {
                final MetaDataRegisterDTO metaData = buildMetaDataDTO(bean, beanShenyuClient,
                        buildApiPath(method, superPath, null), clazz, method, namespaceId);
                getPublisher().publishEvent(metaData);
                getMetaDataMap().put(method, metaData);
            }
        }
    }
    
    @Override
    protected String buildApiPath(final Method method,
                                  final String superPath,
                                  @Nullable final ShenyuDubboClient methodShenyuClient) {
        final String contextPath = this.getContextPath();
        return superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName())
                : pathJoin(contextPath, superPath, Objects.requireNonNull(methodShenyuClient).path());
    }
    
    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final ServiceBean bean,
                                                   @NonNull final ShenyuDubboClient shenyuClient,
                                                   final String path,
                                                   final Class<?> clazz,
                                                   final Method method,
                                                   final String namespaceId) {
        String appName = buildAppName(bean);
        String desc = shenyuClient.desc();
        String serviceName = bean.getInterface();
        String configRuleName = shenyuClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = Optional.ofNullable(method).map(Method::getName).orElseThrow(() -> new ShenyuException("unexpected error"));
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName).collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(getContextPath())
                .host(super.getHost())
                .port(Integer.valueOf(getPort()))
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExt(bean, methodName))
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .enabled(shenyuClient.enabled())
                .namespaceId(namespaceId)
                .build();
    }
    
    @Override
    public String getPort() {
        final String port = super.getPort();
        return getContext().getBeansOfType(ServiceBean.class).entrySet()
                .stream().findFirst().map(entry -> {
                    final ServiceBean<?> serviceBean = entry.getValue();
                    return StringUtils.isBlank(port) || "-1".equals(port)
                            ? String.valueOf(serviceBean.getProtocol().getPort()) : port;
                }).orElse(port);
    }
    
    private String buildRpcExt(final ServiceBean<?> serviceBean, final String methodName) {
        DubboRpcExt build = DubboRpcExt.builder()
                .protocol(StringUtils.isNotEmpty(serviceBean.getProtocol().getName()) ? serviceBean.getProtocol().getName() : "")
                .group(StringUtils.isNotEmpty(serviceBean.getGroup()) ? serviceBean.getGroup() : "")
                .version(StringUtils.isNotEmpty(serviceBean.getVersion()) ? serviceBean.getVersion() : "")
                .loadbalance(StringUtils.isNotEmpty(serviceBean.getLoadbalance()) ? serviceBean.getLoadbalance() : CommonConstants.DEFAULT_LOADBALANCE)
                .retries(Optional.ofNullable(serviceBean.getRetries()).orElse(CommonConstants.DEFAULT_RETRIES))
                .timeout(Optional.ofNullable(serviceBean.getTimeout()).orElse(DEFAULT_CONNECT_TIMEOUT))
                .sent(Optional.ofNullable(serviceBean.getSent()).orElse(Boolean.FALSE))
                .cluster(StringUtils.isNotEmpty(serviceBean.getCluster()) ? serviceBean.getCluster() : Constants.DEFAULT_CLUSTER)
                .url("")
                .build();
        // set method config: loadbalance,retries,timeout,sent
        Optional.ofNullable(serviceBean.getMethods()).orElse(Collections.emptyList()).stream()
                .filter(m -> methodName.equals(m.getName()))
                .findFirst()
                .ifPresent(methodConfig -> {
                    Optional.ofNullable(methodConfig.getLoadbalance()).filter(StringUtils::isNotEmpty).ifPresent(build::setLoadbalance);
                    Optional.ofNullable(methodConfig.getRetries()).ifPresent(build::setRetries);
                    Optional.ofNullable(methodConfig.getTimeout()).ifPresent(build::setTimeout);
                    Optional.ofNullable(methodConfig.getSent()).ifPresent(build::setSent);
                });
        return GsonUtils.getInstance().toJson(build);
    }
}
