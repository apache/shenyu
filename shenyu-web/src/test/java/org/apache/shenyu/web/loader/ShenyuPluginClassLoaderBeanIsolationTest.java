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

package org.apache.shenyu.web.loader;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.annotation.AnnotationDescription;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.stereotype.Component;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShenyuPluginClassLoaderBeanIsolationTest {

    @Test
    void closingOnePluginShouldNotDestroySameSimpleNameFromAnotherPlugin() {
        GenericApplicationContext context = createContext();
        String firstClassName = "fixture.first.SharedComponent";
        String secondClassName = "fixture.second.SharedComponent";
        ShenyuPluginClassLoader firstLoader = createLoader("plugin-one", firstClassName);
        ShenyuPluginClassLoader secondLoader = createLoader("plugin-two", secondClassName);
        try {
            firstLoader.loadUploadedJarPlugins();
            secondLoader.loadUploadedJarPlugins();
            String firstBeanName = firstLoader.getPluginBeanName(firstClassName);
            String secondBeanName = secondLoader.getPluginBeanName(secondClassName);
            assertSame(firstLoader, context.getBean(firstBeanName).getClass().getClassLoader());
            assertSame(secondLoader, context.getBean(secondBeanName).getClass().getClassLoader());

            firstLoader.close();

            assertFalse(context.containsBean(firstBeanName));
            assertTrue(context.containsBean(secondBeanName));
        } finally {
            firstLoader.close();
            secondLoader.close();
            context.close();
        }
    }

    @Test
    void closingPreviousGenerationShouldNotDestroyReplacementBean() {
        GenericApplicationContext context = createContext();
        String className = "fixture.reload.SharedComponent";
        ShenyuPluginClassLoader previousLoader = createLoader("same-plugin", className);
        ShenyuPluginClassLoader replacementLoader = createLoader("same-plugin", className);
        try {
            previousLoader.loadUploadedJarPlugins();
            replacementLoader.loadUploadedJarPlugins();
            String previousBeanName = previousLoader.getPluginBeanName(className);
            String replacementBeanName = replacementLoader.getPluginBeanName(className);

            previousLoader.close();

            assertFalse(context.containsBean(previousBeanName));
            assertTrue(context.containsBean(replacementBeanName));
            assertSame(replacementLoader, context.getBean(replacementBeanName).getClass().getClassLoader());
        } finally {
            previousLoader.close();
            replacementLoader.close();
            context.close();
        }
    }

    @Test
    void failedExistingBeanCreationShouldNotAbortPluginLoading() {
        GenericApplicationContext context = createContext();
        String className = "fixture.recovery.RecoveringComponent";
        ShenyuPluginClassLoader loader = createLoader("recovery-plugin", className);
        String beanName = loader.getPluginBeanName(className);
        context.registerBeanDefinition(beanName, new RootBeanDefinition(FailingBean.class));
        try {
            loader.loadUploadedJarPlugins();

            assertTrue(context.containsBean(beanName));
            assertSame(loader, context.getBean(beanName).getClass().getClassLoader());
        } finally {
            loader.close();
            context.close();
        }
    }

    private GenericApplicationContext createContext() {
        GenericApplicationContext context = new GenericApplicationContext();
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        return context;
    }

    private ShenyuPluginClassLoader createLoader(final String path, final String className) {
        byte[] classBytes = new ByteBuddy()
                .subclass(Object.class)
                .name(className)
                .annotateType(AnnotationDescription.Builder.ofType(Component.class).build())
                .make()
                .getBytes();
        PluginJarParser.PluginJar pluginJar = new PluginJarParser.PluginJar();
        pluginJar.setAbsolutePath(path);
        pluginJar.setClazzMap(Collections.singletonMap(className, classBytes));
        return new ShenyuPluginClassLoader(pluginJar);
    }

    public static final class FailingBean {

        private FailingBean() {
            throw new IllegalStateException("bean creation failed");
        }
    }
}
