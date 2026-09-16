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

package org.apache.shenyu.plugin.api.utils;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.support.GenericApplicationContext;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SpringBeanUtilsTest {

    @Test
    void registerBeanShouldNotReplaceSharedBeanFactoryClassLoader() {
        DefaultListableBeanFactory beanFactory = new DefaultListableBeanFactory();
        final ClassLoader sharedClassLoader = beanFactory.getBeanClassLoader();
        GenericApplicationContext context = new GenericApplicationContext(beanFactory);
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
        beanDefinition.setBeanClassName(TestBean.class.getName());
        ClassLoader pluginClassLoader = new ClassLoader(TestBean.class.getClassLoader()) {
        };

        String beanName = SpringBeanUtils.getInstance().registerBean(beanDefinition, pluginClassLoader);

        assertSame(sharedClassLoader, beanFactory.getBeanClassLoader());
        assertTrue(context.containsBean(beanName));
        assertSame(TestBean.class, beanDefinition.getBeanClass());
        assertSame(TestBean.class, context.getBean(beanName).getClass());
        context.close();
    }

    private static final class TestBean {
    }
}
