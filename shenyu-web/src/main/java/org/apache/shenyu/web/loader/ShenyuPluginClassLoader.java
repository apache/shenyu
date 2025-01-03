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

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * ShenyuUploadPluginClassLoader.
 */
public final class ShenyuPluginClassLoader extends ClassLoader implements Closeable {
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuPluginClassLoader.class);

    static {
        registerAsParallelCapable();
    }

    private final ReentrantLock lock = new ReentrantLock();

    private final Map<String, Class<?>> classCache = new ConcurrentHashMap<>();

    private final Map<String, byte[]> resourceCache = new ConcurrentHashMap<>();

    private final PluginJarParser.PluginJar pluginJar;

    public ShenyuPluginClassLoader(final PluginJarParser.PluginJar pluginJar) {
        super(ShenyuPluginClassLoader.class.getClassLoader());
        this.pluginJar = pluginJar;
    }

    /**
     * checkExistence.
     *
     * @param className className.
     * @return existence
     */
    private boolean checkExistence(final String className) {
        try {
            return Objects.nonNull(this.getParent().loadClass(className));
        } catch (ClassNotFoundException cfe) {
            return false;
        }
    }

    /**
     * loadUploadedJarResourcesList.
     *
     * @return the list
     */
    public List<ShenyuLoaderResult> loadUploadedJarPlugins() {
        List<ShenyuLoaderResult> results = new ArrayList<>();
        Set<String> names = pluginJar.getClazzMap().keySet();
        names.forEach(className -> {
            Object instance;
            try {
                instance = getOrCreateSpringBean(className);
                if (Objects.nonNull(instance)) {
                    results.add(buildResult(instance));
                    LOG.info("The class successfully loaded into a upload-Jar-plugin {} is registered as a spring bean", className);
                }
            } catch (ClassNotFoundException | IllegalAccessException | InstantiationException e) {
                LOG.warn("Registering upload-Jar-plugins succeeds spring bean fails:{}", className, e);
            }
        });
        return results;
    }

    @Override
    public InputStream getResourceAsStream(final String name) {
        byte[] cacheByte = resourceCache.get(name);
        if (Objects.nonNull(cacheByte)) {
            return new ByteArrayInputStream(cacheByte);
        }
        byte[] bytes = pluginJar.getResourceMap().get(name);
        if (Objects.nonNull(bytes)) {
            resourceCache.put(name, bytes);
            return new ByteArrayInputStream(bytes);
        }
        return super.getResourceAsStream(name);
    }

    @Override
    protected Class<?> findClass(final String name) throws ClassNotFoundException {
        if (ability(name)) {
            return this.getParent().loadClass(name);
        }
        Class<?> clazz = classCache.get(name);
        if (Objects.nonNull(clazz)) {
            return clazz;
        }
        synchronized (this) {
            clazz = classCache.get(name);
            if (Objects.isNull(clazz)) {
                // support base64Jar
                if (pluginJar.getClazzMap().containsKey(name) && !checkExistence(name)) {
                    byte[] bytes = pluginJar.getClazzMap().get(name);
                    clazz = defineClass(name, bytes, 0, bytes.length);
                    classCache.put(name, clazz);
                    return clazz;
                }
            }
        }
        throw new ClassNotFoundException(String.format("Class name is %s not found.", name));
    }

    @Override
    public void close() {
        Set<String> clazzNames = pluginJar.getClazzMap().keySet();
        for (String clazzName : clazzNames) {
            SpringBeanUtils.getInstance().destroyBean(clazzName);
        }
    }

    private <T> T getOrCreateSpringBean(final String className) throws ClassNotFoundException, IllegalAccessException, InstantiationException {
        if (SpringBeanUtils.getInstance().existBean(className)) {
            T inst = SpringBeanUtils.getInstance().getBeanByClassName(className);
            // if the class is loaded by other classloader, then reload it
            if (!isLoadedByOtherClassLoader(inst)) {
                return inst;
            }
        }
        lock.lock();
        try {
            T inst = SpringBeanUtils.getInstance().getBeanByClassName(className);
            if (Objects.isNull(inst) || isLoadedByOtherClassLoader(inst)) {
                Class<?> clazz = Class.forName(className, false, this);
                //Exclude ShenyuPlugin subclass and PluginDataHandler subclass
                // without adding @Component @Service annotation
                boolean next = ShenyuPlugin.class.isAssignableFrom(clazz)
                        || PluginDataHandler.class.isAssignableFrom(clazz);
                if (!next) {
                    Annotation[] annotations = clazz.getAnnotations();
                    next = Arrays.stream(annotations).anyMatch(e -> e.annotationType().equals(Component.class)
                            || e.annotationType().equals(Service.class));
                }
                if (next) {
                    GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
                    beanDefinition.setBeanClassName(className);
                    beanDefinition.setAutowireCandidate(true);
                    beanDefinition.setRole(BeanDefinition.ROLE_INFRASTRUCTURE);
                    String beanName = SpringBeanUtils.getInstance().registerBean(beanDefinition, this);
                    inst = SpringBeanUtils.getInstance().getBeanByClassName(beanName);
                }
            }
            return inst;
        } finally {
            lock.unlock();
        }
    }

    /**
     * whether the class is loaded by other classloader.
     *
     * @param inst instance
     * @param <T>  the type parameter
     * @return boolean
     */
    private <T> boolean isLoadedByOtherClassLoader(final T inst) {
        return !inst.getClass().getClassLoader().equals(this);
    }

    private ShenyuLoaderResult buildResult(final Object instance) {
        ShenyuLoaderResult result = new ShenyuLoaderResult();
        if (instance instanceof ShenyuPlugin) {
            result.setShenyuPlugin((ShenyuPlugin) instance);
        } else if (instance instanceof PluginDataHandler) {
            result.setPluginDataHandler((PluginDataHandler) instance);
        }
        return result;
    }

    private boolean ability(final String name) {
        return !pluginJar.getClazzMap().containsKey(name);
    }

    /**
     * compareVersion.
     *
     * @param version version
     * @return boolean
     */
    public boolean compareVersion(final String version) {
        return pluginJar.getVersion().equals(version);
    }

}
