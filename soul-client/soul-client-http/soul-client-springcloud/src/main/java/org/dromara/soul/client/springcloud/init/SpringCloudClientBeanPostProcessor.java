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

package org.dromara.soul.client.springcloud.init;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.springcloud.annotation.SoulSpringCloudClient;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.dto.SpringCloudRegisterDTO;
import org.dromara.soul.client.springcloud.utils.ValidateUtils;
import org.dromara.soul.common.enums.RpcTypeEnum;
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
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * The type Soul client bean post processor.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class SpringCloudClientBeanPostProcessor implements BeanPostProcessor {

    private final ThreadPoolExecutor executorService;

    private final String url;

    private final SoulSpringCloudConfig config;

    private final Environment env;

    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param config the soul spring cloud config
     * @param env    the env
     */
    public SpringCloudClientBeanPostProcessor(final SoulSpringCloudConfig config, final Environment env) {
        ValidateUtils.validate(config, env);
        this.config = config;
        this.env = env;
        this.url = config.getAdminUrl() + "/soul-client/springcloud-register";
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (config.isFull()) {
            return bean;
        }
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String prePath = "";
            SoulSpringCloudClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), SoulSpringCloudClient.class);
            if (Objects.nonNull(clazzAnnotation)) {
                if (clazzAnnotation.path().indexOf("*") > 1) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> RegisterUtils.doRegister(buildJsonParams(clazzAnnotation, finalPrePath), url,
                            RpcTypeEnum.SPRING_CLOUD));
                    return bean;
                }
                prePath = clazzAnnotation.path();
            }
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
            for (Method method : methods) {
                SoulSpringCloudClient soulSpringCloudClient = AnnotationUtils.findAnnotation(method, SoulSpringCloudClient.class);
                if (Objects.nonNull(soulSpringCloudClient)) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> RegisterUtils.doRegister(buildJsonParams(soulSpringCloudClient, finalPrePath), url,
                            RpcTypeEnum.SPRING_CLOUD));
                }
            }
        }
        return bean;
    }

    private String buildJsonParams(final SoulSpringCloudClient soulSpringCloudClient, final String prePath) {
        String contextPath = config.getContextPath();
        String appName = env.getProperty("spring.application.name");
        String path = contextPath + prePath + soulSpringCloudClient.path();
        String desc = soulSpringCloudClient.desc();
        String configRuleName = soulSpringCloudClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        SpringCloudRegisterDTO registerDTO = SpringCloudRegisterDTO.builder()
                .context(contextPath)
                .appName(appName)
                .path(path)
                .pathDesc(desc)
                .rpcType(soulSpringCloudClient.rpcType())
                .enabled(soulSpringCloudClient.enabled())
                .ruleName(ruleName)
                .build();
        return OkHttpTools.getInstance().getGson().toJson(registerDTO);
    }
}


