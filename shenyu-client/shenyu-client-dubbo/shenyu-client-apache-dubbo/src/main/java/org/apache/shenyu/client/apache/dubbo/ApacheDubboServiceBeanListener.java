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

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.Constants;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.client.dubbo.common.dto.DubboRpcExt;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * The Apache Dubbo ServiceBean Listener.
 */
@SuppressWarnings("all")
public class ApacheDubboServiceBeanListener implements ApplicationListener<ContextRefreshedEvent> {

    private ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private ExecutorService executorService;

    private String contextPath;

    private String appName;

    private final String host;

    private final String port;

    public ApacheDubboServiceBeanListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        String appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        if (StringUtils.isBlank(contextPath)) {
            throw new ShenyuClientIllegalArgumentException("apache dubbo client must config the contextPath or appName");
        }
        this.contextPath = contextPath;
        this.appName = appName;
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        this.port = props.getProperty(ShenyuClientConstants.PORT);
        executorService = Executors.newSingleThreadExecutor(new ThreadFactoryBuilder().setNameFormat("shenyu-apache-dubbo-client-thread-pool-%d").build());
        publisher.start(shenyuClientRegisterRepository);
    }
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        // Fix bug(https://github.com/dromara/shenyu/issues/415), upload dubbo metadata on ContextRefreshedEvent
        Map<String, ServiceBean> serviceBean = contextRefreshedEvent.getApplicationContext().getBeansOfType(ServiceBean.class);
        for (Map.Entry<String, ServiceBean> entry : serviceBean.entrySet()) {
            handler(entry.getValue());
        }
        serviceBean.values().stream().findFirst().ifPresent(bean -> {
            publisher.publishEvent(buildURIRegisterDTO(bean));
        });
    }

    private void handler(final ServiceBean<?> serviceBean) {
        Object refProxy = serviceBean.getRef();
        Class<?> clazz = refProxy.getClass();
        if (AopUtils.isAopProxy(refProxy)) {
            clazz = AopUtils.getTargetClass(refProxy);
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuDubboClient shenyuDubboClient = method.getAnnotation(ShenyuDubboClient.class);
            if (Objects.nonNull(shenyuDubboClient)) {
                publisher.publishEvent(buildMetaDataDTO(serviceBean, shenyuDubboClient, method));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final ServiceBean<?> serviceBean, final ShenyuDubboClient shenyuDubboClient, final Method method) {
        String appName = buildAppName(serviceBean);
        String path = contextPath + shenyuDubboClient.path();
        String desc = shenyuDubboClient.desc();
        String serviceName = serviceBean.getInterface();
        String configRuleName = shenyuDubboClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName).collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(contextPath)
                .host(buildHost())
                .port(buildPort(serviceBean))
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExt(serviceBean))
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .enabled(shenyuDubboClient.enabled())
                .build();
    }
    
    private URIRegisterDTO buildURIRegisterDTO(final ServiceBean serviceBean) {
        return URIRegisterDTO.builder()
                .contextPath(this.contextPath)
                .appName(buildAppName(serviceBean))
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .host(buildHost())
                .port(buildPort(serviceBean))
                .build();
    }

    private String buildRpcExt(final ServiceBean serviceBean) {
        DubboRpcExt build = DubboRpcExt.builder()
                .group(StringUtils.isNotEmpty(serviceBean.getGroup()) ? serviceBean.getGroup() : "")
                .version(StringUtils.isNotEmpty(serviceBean.getVersion()) ? serviceBean.getVersion() : "")
                .loadbalance(StringUtils.isNotEmpty(serviceBean.getLoadbalance()) ? serviceBean.getLoadbalance() : Constants.DEFAULT_LOADBALANCE)
                .retries(Objects.isNull(serviceBean.getRetries()) ? Constants.DEFAULT_RETRIES : serviceBean.getRetries())
                .timeout(Objects.isNull(serviceBean.getTimeout()) ? Constants.DEFAULT_CONNECT_TIMEOUT : serviceBean.getTimeout())
                .sent(Objects.isNull(serviceBean.getSent()) ? Constants.DEFAULT_SENT : serviceBean.getSent())
                .cluster(StringUtils.isNotEmpty(serviceBean.getCluster()) ? serviceBean.getCluster() : Constants.DEFAULT_CLUSTER)
                .url("")
                .build();
        return GsonUtils.getInstance().toJson(build);
    }
    
    private String buildAppName(final ServiceBean serviceBean) {
        return StringUtils.isBlank(this.appName) ? serviceBean.getApplication().getName() : this.appName;
    }
    
    private String buildHost() {
        return IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
    }
    
    private int buildPort(final ServiceBean serviceBean) {
        return StringUtils.isBlank(this.port) ? serviceBean.getProtocol().getPort() : Integer.parseInt(this.port);
    }
}
