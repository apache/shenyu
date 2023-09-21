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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.config.ShenyuConfig.ExtPlugin;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.net.URLClassLoader;
import java.util.*;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

/**
 * The type Shenyu loader service.
 */
public class ShenyuLoaderService {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuLoaderService.class);

    private final ShenyuWebHandler webHandler;

    private final CommonPluginDataSubscriber subscriber;

    private final ShenyuConfig shenyuConfig;

    /**
     * Instantiates a new Shenyu loader service.
     *
     * @param webHandler   the web handler
     * @param subscriber   the subscriber
     * @param shenyuConfig the shenyu config
     */
    public ShenyuLoaderService(final ShenyuWebHandler webHandler, final CommonPluginDataSubscriber subscriber, final ShenyuConfig shenyuConfig) {
        this.subscriber = subscriber;
        this.webHandler = webHandler;
        this.shenyuConfig = shenyuConfig;
        ExtPlugin config = shenyuConfig.getExtPlugin();
        if (config.getEnabled()) {
            ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(config.getThreads(), ShenyuThreadFactory.create("plugin-ext-loader", true));
            executor.scheduleAtFixedRate(this::loaderExtPlugins, config.getScheduleDelay(), config.getScheduleTime(), TimeUnit.SECONDS);
        }
    }

    private void loaderExtPlugins() {
        try {
            List<PluginJarParser.PluginJar> uploadPluginJars = ShenyuExtPathPluginJarLoader.loadExtendPlugins(shenyuConfig.getExtPlugin().getPath());
            List<ShenyuLoaderResult> extendPlugins = new ArrayList<>();
            for (PluginJarParser.PluginJar extPath : uploadPluginJars) {
                LOG.info("shenyu extPlugin find new {} to load", extPath.getAbsolutePath());
                ShenyuPluginClassLoader extPathClassLoader = ShenyuPluginClassloaderHolder.getSingleton().createExtPathClassLoader(extPath);
                extendPlugins.addAll(extPathClassLoader.loadUploadedJarPlugins());
            }
            loaderPlugins(extendPlugins);
        } catch (Exception e) {
            LOG.error("shenyu ext plugins load has error ", e);
        }
    }

    /**
     * loadUploadedJarPlugins.
     *
     * @param uploadedJarResource uploadedJarResource
     */
    public void loadUploadedJarPlugins(final PluginData uploadedJarResource) {
        try {
            PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(Base64.getDecoder().decode(uploadedJarResource.getPluginJar()));
            ShenyuPluginClassLoader shenyuPluginClassLoader = ShenyuPluginClassloaderHolder.getSingleton().getUploadClassLoader(pluginJar);
            if (Objects.nonNull(shenyuPluginClassLoader) && shenyuPluginClassLoader.compareVersion(pluginJar.getVersion())) {
                LOG.info("shenyu uploadPlugin has same version don't reload it");
                return;
            }
            shenyuPluginClassLoader = ShenyuPluginClassloaderHolder.getSingleton().recreateUploadClassLoader(pluginJar);
            List<ShenyuLoaderResult> uploadPlugins = shenyuPluginClassLoader.loadUploadedJarPlugins();
            loaderPlugins(uploadPlugins);
        } catch (Exception e) {
            LOG.error("shenyu upload plugins load has error ", e);
        }
    }

    /**
     * loaderPlugins.
     *
     * @param results results
     */
    public void loaderPlugins(final List<ShenyuLoaderResult> results) {
        if (CollectionUtils.isEmpty(results)) {
            return;
        }
        List<ShenyuPlugin> shenyuExtendPlugins = results.stream().map(ShenyuLoaderResult::getShenyuPlugin).filter(Objects::nonNull).collect(Collectors.toList());
        webHandler.putExtPlugins(shenyuExtendPlugins);
        List<PluginDataHandler> handlers = results.stream().map(ShenyuLoaderResult::getPluginDataHandler).filter(Objects::nonNull).collect(Collectors.toList());
        subscriber.putExtendPluginDataHandler(handlers);
    }

    public void loaderPlugins(final List<String> registerClassNames, URLClassLoader classLoader) throws ClassNotFoundException, IllegalAccessException, InstantiationException {
        if (Objects.isNull(registerClassNames)) {
            return;
        }

        for (String className : registerClassNames) {
            Class<?> clazz = Class.forName(className, false, classLoader);
            if (ShenyuPlugin.class.isAssignableFrom(clazz)) {
                AbstractShenyuPlugin plugin = getOrCreateSpringBean(className, classLoader);
                plugin.setClassLoader(classLoader);
                webHandler.putExtPlugins(Arrays.asList(plugin));
            } else if (PluginDataHandler.class.isAssignableFrom(clazz)) {
                PluginDataHandler pluginDataHandler = getOrCreateSpringBean(className, classLoader);
                subscriber.putExtendPluginDataHandler(Arrays.asList(pluginDataHandler));
            }
        }
    }

    private final ReentrantLock lock = new ReentrantLock();

    private <T> T getOrCreateSpringBean(final String className, final ClassLoader classLoader) throws ClassNotFoundException, IllegalAccessException, InstantiationException {
        if (SpringBeanUtils.getInstance().existBean(className)) {
            return SpringBeanUtils.getInstance().getBeanByClassName(className);
        }
        lock.lock();
        try {
            T inst = SpringBeanUtils.getInstance().getBeanByClassName(className);
            if (Objects.isNull(inst)) {
                Class<?> clazz = Class.forName(className, false, classLoader);
                //Exclude ShenyuPlugin subclass and PluginDataHandler subclass
                // without adding @Component @Service annotation
                boolean next = ShenyuPlugin.class.isAssignableFrom(clazz)
                        || PluginDataHandler.class.isAssignableFrom(clazz);
                if (next) {
                    GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
                    beanDefinition.setBeanClassName(className);
                    beanDefinition.setAutowireCandidate(true);
                    beanDefinition.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                    String beanName = SpringBeanUtils.getInstance().registerBean(beanDefinition, classLoader);
                    inst = SpringBeanUtils.getInstance().getBeanByClassName(beanName);
                }
            }
            return inst;
        } finally {
            lock.unlock();
        }
    }
}
