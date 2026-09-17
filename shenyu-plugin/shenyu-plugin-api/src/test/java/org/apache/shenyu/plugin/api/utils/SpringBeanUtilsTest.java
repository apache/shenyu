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
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.support.GenericApplicationContext;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Proxy;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SpringBeanUtilsTest {

    @Test
    void registerBeanShouldUsePluginClassLoaderWithoutReplacingSharedLoader() throws IOException {
        DefaultListableBeanFactory beanFactory = new DefaultListableBeanFactory();
        final ClassLoader sharedClassLoader = beanFactory.getBeanClassLoader();
        GenericApplicationContext context = new GenericApplicationContext(beanFactory);
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        GenericBeanDefinition delegate = new GenericBeanDefinition();
        delegate.setBeanClassName(TestBean.class.getName());
        BeanDefinition beanDefinition = (BeanDefinition) Proxy.newProxyInstance(getClass().getClassLoader(),
                new Class[]{BeanDefinition.class}, (proxy, method, args) -> method.invoke(delegate, args));
        ClassLoader pluginClassLoader = new PluginTestClassLoader(TestBean.class);

        String beanName = SpringBeanUtils.getInstance().registerBean(beanDefinition, pluginClassLoader);

        assertSame(sharedClassLoader, beanFactory.getBeanClassLoader());
        assertTrue(context.containsBean(beanName));
        assertSame(pluginClassLoader, context.getBean(beanName).getClass().getClassLoader());
        context.close();
    }

    public static final class TestBean {
    }

    private static final class PluginTestClassLoader extends ClassLoader {

        private final String className;

        private final byte[] classBytes;

        private PluginTestClassLoader(final Class<?> targetClass) throws IOException {
            super(targetClass.getClassLoader());
            className = targetClass.getName();
            String resourceName = "/" + className.replace('.', '/') + ".class";
            try (InputStream input = targetClass.getResourceAsStream(resourceName)) {
                classBytes = input.readAllBytes();
            }
        }

        @Override
        protected synchronized Class<?> loadClass(final String name, final boolean resolve) throws ClassNotFoundException {
            if (!className.equals(name)) {
                return super.loadClass(name, resolve);
            }
            Class<?> loaded = findLoadedClass(name);
            if (Objects.isNull(loaded)) {
                loaded = defineClass(name, classBytes, 0, classBytes.length);
            }
            if (resolve) {
                resolveClass(loaded);
            }
            return loaded;
        }
    }
}
