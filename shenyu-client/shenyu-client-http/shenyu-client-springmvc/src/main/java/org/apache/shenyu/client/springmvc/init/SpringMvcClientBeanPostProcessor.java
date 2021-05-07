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

package org.apache.shenyu.client.springmvc.init;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.apache.shenyu.client.springmvc.annotation.SoulSpringMvcClient;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.SoulClientRegisterRepository;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type Soul spring mvc client bean post processor.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class SpringMvcClientBeanPostProcessor implements BeanPostProcessor {
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();
    
    private final ThreadPoolExecutor executorService;
    
    private final String contextPath;
    
    private final String appName;
    
    private final String host;
    
    private final Integer port;
    
    private final Boolean isFull;
    
    /**
     * Instantiates a new Soul client bean post processor.
     */
    public SpringMvcClientBeanPostProcessor(final SoulRegisterCenterConfig config, final SoulClientRegisterRepository soulClientRegisterRepository) {
        String registerType = config.getRegisterType();
        String serverLists = config.getServerLists();
        Properties props = config.getProps();
        int port = Integer.parseInt(props.getProperty("port"));
        if (StringUtils.isBlank(registerType) || StringUtils.isBlank(serverLists) || port <= 0) {
            String errorMsg = "http register param must config the registerType , serverLists and port must > 0";
            log.error(errorMsg);
            throw new RuntimeException(errorMsg);
        }
        this.appName = props.getProperty("appName");
        this.host = props.getProperty("host");
        this.port = port;
        this.contextPath = props.getProperty("contextPath");
        this.isFull = Boolean.parseBoolean(props.getProperty("isFull", "false"));
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
        publisher.start(soulClientRegisterRepository);
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (isFull) {
            return bean;
        }
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || requestMapping != null) {
            SoulSpringMvcClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), SoulSpringMvcClient.class);
            String prePath = "";
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
                SoulSpringMvcClient soulSpringMvcClient = AnnotationUtils.findAnnotation(method, SoulSpringMvcClient.class);
                if (Objects.nonNull(soulSpringMvcClient)) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> publisher.publishEvent(buildMetaDataDTO(soulSpringMvcClient, finalPrePath)));
                }
            }
        }
        return bean;
    }
    
    private MetaDataRegisterDTO buildMetaDataDTO(final SoulSpringMvcClient soulSpringMvcClient, final String prePath) {
        String contextPath = this.contextPath;
        String appName = this.appName;
        Integer port = this.port;
        String path;
        if (StringUtils.isEmpty(contextPath)) {
            path = prePath + soulSpringMvcClient.path();
        } else {
            path = contextPath + prePath + soulSpringMvcClient.path();
        }
        String desc = soulSpringMvcClient.desc();
        String configHost = this.host;
        String host = StringUtils.isBlank(configHost) ? IpUtils.getHost() : configHost;
        String configRuleName = soulSpringMvcClient.ruleName();
        String ruleName = StringUtils.isBlank(configRuleName) ? path : configRuleName;
        return MetaDataRegisterDTO.builder()
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .path(path)
                .pathDesc(desc)
                .rpcType(soulSpringMvcClient.rpcType())
                .enabled(soulSpringMvcClient.enabled())
                .ruleName(ruleName)
                .registerMetaData(soulSpringMvcClient.registerMetaData())
                .build();
    }
}


