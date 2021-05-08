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

package org.apache.shenyu.client.alibaba.dubbo;

import com.alibaba.dubbo.common.Constants;
import com.alibaba.dubbo.common.utils.StringUtils;
import com.alibaba.dubbo.config.spring.ServiceBean;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.apache.shenyu.client.dubbo.common.annotation.SoulDubboClient;
import org.apache.shenyu.client.dubbo.common.dto.DubboRpcExt;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.SoulClientRegisterRepository;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
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
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * The Alibaba Dubbo ServiceBean Listener.
 *
 * @author xiaoyu
 */
@Slf4j
@SuppressWarnings("all")
public class AlibabaDubboServiceBeanListener implements ApplicationListener<ContextRefreshedEvent> {
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();
    
    private AtomicBoolean registered = new AtomicBoolean(false);
    
    private final ExecutorService executorService;
    
    private final String contextPath;
    
    private final String appName;

    private final String host;

    private final String port;

    public AlibabaDubboServiceBeanListener(final SoulRegisterCenterConfig config, final SoulClientRegisterRepository soulClientRegisterRepository) {
        Properties props = config.getProps();
        String contextPath = props.getProperty("contextPath");
        String appName = props.getProperty("appName");
        if (StringUtils.isEmpty(contextPath)) {
            throw new RuntimeException("apache dubbo client must config the contextPath");
        }
        this.contextPath = contextPath;
        this.appName = appName;
        this.host = props.getProperty("host");
        this.port = props.getProperty("port");
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
        publisher.start(soulClientRegisterRepository);
    }

    private void handler(final ServiceBean<?> serviceBean) {
        Class<?> clazz = serviceBean.getRef().getClass();
        if (AopUtils.isCglibProxy(clazz)) {
            String superClassName = clazz.getGenericSuperclass().getTypeName();
            try {
                clazz = Class.forName(superClassName);
            } catch (ClassNotFoundException e) {
                log.error(String.format("class not found: %s", superClassName));
                return;
            }
        }
        if (AopUtils.isJdkDynamicProxy(clazz)) {
            clazz = AopUtils.getTargetClass(serviceBean.getRef());
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulDubboClient soulDubboClient = method.getAnnotation(SoulDubboClient.class);
            if (Objects.nonNull(soulDubboClient)) {
                publisher.publishEvent(buildMetaDataDTO(serviceBean, soulDubboClient, method));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final ServiceBean<?> serviceBean, final SoulDubboClient soulDubboClient, final Method method) {
        String appName = this.appName;
        if (StringUtils.isEmpty(appName)) {
            appName = serviceBean.getApplication().getName();
        }
        String path = contextPath + soulDubboClient.path();
        String desc = soulDubboClient.desc();
        String serviceName = serviceBean.getInterface();
        String host = StringUtils.isBlank(this.host) ? IpUtils.getHost() : this.host;
        int port = StringUtils.isBlank(this.port) ? -1 : Integer.parseInt(this.port);
        String configRuleName = soulDubboClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExt(serviceBean))
                .rpcType("dubbo")
                .enabled(soulDubboClient.enabled())
                .build();
    }

    private String buildRpcExt(final ServiceBean<?> serviceBean) {
        DubboRpcExt builder = DubboRpcExt.builder()
                .group(StringUtils.isNotEmpty(serviceBean.getGroup()) ? serviceBean.getGroup() : "")
                .version(StringUtils.isNotEmpty(serviceBean.getVersion()) ? serviceBean.getVersion() : "")
                .loadbalance(StringUtils.isNotEmpty(serviceBean.getLoadbalance()) ? serviceBean.getLoadbalance() : Constants.DEFAULT_LOADBALANCE)
                .retries(Objects.isNull(serviceBean.getRetries()) ? Constants.DEFAULT_RETRIES : serviceBean.getRetries())
                .timeout(Objects.isNull(serviceBean.getTimeout()) ? Constants.DEFAULT_CONNECT_TIMEOUT : serviceBean.getTimeout())
                .url("")
                .build();
        return GsonUtils.getInstance().toJson(builder);

    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        // Fix bug(https://github.com/dromara/soul/issues/415), upload dubbo metadata on ContextRefreshedEvent
        Map<String, ServiceBean> serviceBean = contextRefreshedEvent.getApplicationContext().getBeansOfType(ServiceBean.class);
        for (Map.Entry<String, ServiceBean> entry : serviceBean.entrySet()) {
            executorService.execute(() -> handler(entry.getValue()));
        }
    }
}
