/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.client.dubbo.spring;

import org.apache.dubbo.config.spring.ServiceBean;
import org.dromara.soul.client.api.annotation.SoulClient;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.Objects;

/**
 * The type Dubbo service bean post processor.
 *
 * @author xiaoyu
 */
public class DubboServiceBeanPostProcessor implements BeanPostProcessor {

    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
        return bean;
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
        if (bean instanceof ServiceBean) {
            ServiceBean serviceBean = (ServiceBean) bean;
            Class<?> clazz = serviceBean.getRef().getClass();
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
            for (Method method : methods) {
                SoulClient soulClient = method.getAnnotation(SoulClient.class);
                /*SoulClient soulClient = AnnotationUtils.findAnnotation(method, SoulClient.class);*/
                if (Objects.nonNull(soulClient)) {

                }
            }
        }
        return bean;
    }
}