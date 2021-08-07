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

package org.apache.shenyu.client.springcloud.init;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * The type Shenyu client bean post processor.
 */
public class SpringCloudClientBeanPostProcessor implements BeanPostProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudClientBeanPostProcessor.class);

    private ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final ExecutorService executorService;

    private final String contextPath;

    private final Boolean isFull;

    private final Environment env;

    private final String host;

    private final String port;

    private final String servletContextPath;

    /**
     * Instantiates a new Shenyu client bean post processor.
     *
     * @param config the shenyu spring cloud config
     * @param env    the env
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public SpringCloudClientBeanPostProcessor(final ShenyuRegisterCenterConfig config, final Environment env, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        String registerType = config.getRegisterType();
        String serverLists = config.getServerLists();
        String appName = env.getProperty("spring.application.name");
        if (StringUtils.isBlank(registerType) || StringUtils.isBlank(serverLists) || StringUtils.isBlank(appName)) {
            String errorMsg = "spring cloud param must config the registerType , serverLists  and appName";
            LOG.error(errorMsg);
            throw new RuntimeException(errorMsg);
        }
        executorService = Executors.newSingleThreadExecutor(new ThreadFactoryBuilder().setNameFormat("shenyu-spring-cloud-client-thread-pool-%d").build());
        this.env = env;
        Properties props = config.getProps();
        this.contextPath = props.getProperty("contextPath");
        this.isFull = Boolean.parseBoolean(props.getProperty("isFull", "false"));
        this.host = config.getProps().getProperty("host");
        this.port = config.getProps().getProperty("port");
        this.servletContextPath = env.getProperty("server.servlet.context-path", "");
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (isFull) {
            return bean;
        }
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String prePath = "";
            ShenyuSpringCloudClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), ShenyuSpringCloudClient.class);
            if (Objects.isNull(clazzAnnotation)) {
                return bean;
            }
            if (clazzAnnotation.path().indexOf("*") > 1) {
                String finalPrePath = prePath;
                executorService.execute(() -> publisher.publishEvent(buildMetaDataDTO(clazzAnnotation, finalPrePath)));
                return bean;
            }
            prePath = clazzAnnotation.path();
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
            for (Method method : methods) {
                ShenyuSpringCloudClient shenyuSpringCloudClient = AnnotationUtils.findAnnotation(method, ShenyuSpringCloudClient.class);
                if (Objects.nonNull(shenyuSpringCloudClient)) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> publisher.publishEvent(buildMetaDataDTO(shenyuSpringCloudClient, finalPrePath)));
                }
            }
        }
        return bean;
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final ShenyuSpringCloudClient shenyuSpringCloudClient, final String prePath) {
        String contextPath = StringUtils.isBlank(this.contextPath) ? this.servletContextPath : this.contextPath;
        String appName = env.getProperty("spring.application.name");
        String path = contextPath + prePath + shenyuSpringCloudClient.path();
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        int port = StringUtils.isBlank(this.port) ? -1 : Integer.parseInt(this.port);
        String desc = shenyuSpringCloudClient.desc();
        String configRuleName = shenyuSpringCloudClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        return MetaDataRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .host(host)
                .port(port)
                .path(path)
                .pathDesc(desc)
                .rpcType(shenyuSpringCloudClient.rpcType())
                .enabled(shenyuSpringCloudClient.enabled())
                .ruleName(ruleName)
                .build();
    }
}


