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

import com.google.common.collect.Lists;
import com.google.common.io.ByteStreams;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * Shenyu Plugin loader.
 */
public final class ShenyuPluginLoader extends ClassLoader implements Closeable {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuPluginLoader.class);

    static {
        registerAsParallelCapable();
    }

    private static volatile ShenyuPluginLoader pluginLoader;

    private final ReentrantLock lock = new ReentrantLock();

    private final List<PluginJar> jars = Lists.newArrayList();

    private final Set<String> names = new HashSet<>();

    private final Map<String, Class<?>> classCache = new ConcurrentHashMap<>();

    private final Map<String, byte[]> uploadedJarClassByteArrayCache = new ConcurrentHashMap<>();

    private ShenyuPluginLoader() {
        super(ShenyuPluginLoader.class.getClassLoader());
    }

    /**
     * Get plugin loader instance.
     *
     * @return plugin loader instance
     */
    public static ShenyuPluginLoader getInstance() {
        if (null == pluginLoader) {
            synchronized (ShenyuPluginLoader.class) {
                if (null == pluginLoader) {
                    pluginLoader = new ShenyuPluginLoader();
                }
            }
        }
        return pluginLoader;
    }

    /**
     * Load extend plugins list.
     *
     * @param path the path
     * @return the list
     * @throws IOException the io exception
     */
    public List<ShenyuLoaderResult> loadExtendPlugins(final String path) throws IOException {
        File[] jarFiles = ShenyuPluginPathBuilder.getPluginFile(path).listFiles(file -> file.getName().endsWith(".jar"));
        if (Objects.isNull(jarFiles)) {
            return Collections.emptyList();
        }
        List<ShenyuLoaderResult> results = new ArrayList<>();
        boolean loadNewPlugin = false;
        for (File each : jarFiles) {
            if (jars.stream().map(PluginJar::absolutePath).filter(StringUtils::hasText).anyMatch(p -> p.equals(each.getAbsolutePath()))) {
                continue;
            }
            loadNewPlugin = true;
            JarFile jar = new JarFile(each, true);
            jars.add(new PluginJar(jar, each));
            Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                JarEntry jarEntry = entries.nextElement();
                String entryName = jarEntry.getName();
                if (entryName.endsWith(".class") && !entryName.contains("$")) {
                    String className = entryName.substring(0, entryName.length() - 6).replaceAll("/", ".");
                    names.add(className);
                }
            }
        }

        if (!loadNewPlugin) {
            return results;
        }

        names.forEach(className -> {
            Object instance;
            try {
                if (!uploadedJarClassByteArrayCache.containsKey(className)) {
                    instance = getOrCreateSpringBean(className);
                    if (Objects.nonNull(instance)) {
                        results.add(buildResult(instance));
                        LOG.info("The class successfully loaded into a ext-plugin {} is registered as a spring bean", className);
                    }
                }
            } catch (ClassNotFoundException | IllegalAccessException | InstantiationException e) {
                LOG.warn("Registering ext-plugins succeeds spring bean fails:{}", className);
            }
        });
        return results;
    }

    /**
     * loadUploadedJarResourcesList.
     *
     * @param loadUploadedJarResources loadUploadedJarResources
     * @return the list
     */
    public List<ShenyuLoaderResult> loadUploadedJarPlugins(final List<String> loadUploadedJarResources) {
        List<byte[]> jarByteArrayList = loadUploadedJarResources.stream().map(loadUploadedJarResourceStr -> Base64.getDecoder().decode(loadUploadedJarResourceStr)).collect(Collectors.toList());
        for (byte[] jarByteArray : jarByteArrayList) {
            parserJar(jarByteArray);
        }
        List<ShenyuLoaderResult> results = new ArrayList<>();
        names.forEach(className -> {
            Object instance;
            try {
                instance = getOrCreateSpringBean(className);
                if (Objects.nonNull(instance)) {
                    results.add(buildResult(instance));
                    LOG.info("The class successfully loaded into a upload-Jar-plugin {} is registered as a spring bean", className);
                }
            } catch (ClassNotFoundException | IllegalAccessException | InstantiationException e) {
                LOG.warn("Registering upload-Jar-plugins succeeds spring bean fails:{}", className);
            }
        });
        return results;
    }

    /**
     * parserJar.
     *
     * @param jarBytes jarBytes
     */
    private void parserJar(final byte[] jarBytes) {
        try (JarInputStream jarInputStream = new JarInputStream(new ByteArrayInputStream(jarBytes))) {
            JarEntry jarEntry;
            while ((jarEntry = jarInputStream.getNextJarEntry()) != null) {
                String entryName = jarEntry.getName();
                if (!jarEntry.isDirectory() && entryName.endsWith(".class") && !entryName.contains("$")) {
                    String className = jarEntry.getName().substring(0, entryName.length() - 6).replaceAll("/", ".");
                    try (ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {
                        int data;
                        while ((data = jarInputStream.read()) != -1) {
                            buffer.write(data);
                        }
                        buffer.flush();
                        byte[] classByteArray = buffer.toByteArray();
                        names.add(className);
                        uploadedJarClassByteArrayCache.put(className, classByteArray);
                    }
                }
            }
        } catch (IOException e) {
            throw new ShenyuException("load jar classes find error");
        }
    }

    @Override
    protected Class<?> findClass(final String name) throws ClassNotFoundException {
        if (ability(name)) {
            return this.getParent().loadClass(name);
        }
        Class<?> clazz = classCache.get(name);
        if (clazz != null) {
            return clazz;
        }
        synchronized (this) {
            clazz = classCache.get(name);
            if (clazz == null) {
                // support base64Jar
                if (uploadedJarClassByteArrayCache.containsKey(name)) {
                    byte[] currClazzByteArray = uploadedJarClassByteArrayCache.remove(name);
                    clazz = defineClass(name, currClazzByteArray, 0, currClazzByteArray.length);
                    classCache.put(name, clazz);
                    return clazz;
                } else {
                    String path = classNameToPath(name);
                    for (PluginJar each : jars) {
                        ZipEntry entry = each.jarFile.getEntry(path);
                        if (Objects.nonNull(entry)) {
                            try {
                                int index = name.lastIndexOf('.');
                                if (index != -1) {
                                    String packageName = name.substring(0, index);
                                    definePackageInternal(packageName, each.jarFile.getManifest());
                                }
                                byte[] data = ByteStreams.toByteArray(each.jarFile.getInputStream(entry));
                                clazz = defineClass(name, data, 0, data.length);
                                classCache.put(name, clazz);
                                return clazz;
                            } catch (final IOException ex) {
                                LOG.error("Failed to load class {}.", name, ex);
                            }
                        }
                    }
                }
            }
        }
        throw new ClassNotFoundException(String.format("Class name is %s not found.", name));
    }

    @Override
    protected Enumeration<URL> findResources(final String name) throws IOException {
        if (ability(name)) {
            return this.getParent().getResources(name);
        }
        List<URL> resources = Lists.newArrayList();
        for (PluginJar each : jars) {
            JarEntry entry = each.jarFile.getJarEntry(name);
            if (Objects.nonNull(entry)) {
                try {
                    resources.add(new URL(String.format("jar:file:%s!/%s", each.sourcePath.getAbsolutePath(), name)));
                } catch (final MalformedURLException ignored) {
                }
            }
        }
        return Collections.enumeration(resources);
    }

    @Override
    protected URL findResource(final String name) {
        if (ability(name)) {
            return this.getParent().getResource(name);
        }
        for (PluginJar each : jars) {
            JarEntry entry = each.jarFile.getJarEntry(name);
            if (Objects.nonNull(entry)) {
                try {
                    return new URL(String.format("jar:file:%s!/%s", each.sourcePath.getAbsolutePath(), name));
                } catch (final MalformedURLException ignored) {
                }
            }
        }
        return null;
    }

    @Override
    public void close() {
        for (PluginJar each : jars) {
            try {
                each.jarFile.close();
            } catch (final IOException ex) {
                LOG.error("close shenyu plugin jar is ", ex);
            }
        }
    }

    private <T> T getOrCreateSpringBean(final String className) throws ClassNotFoundException, IllegalAccessException, InstantiationException {
        if (SpringBeanUtils.getInstance().existBean(className)) {
            return SpringBeanUtils.getInstance().getBeanByClassName(className);
        }
        lock.lock();
        try {
            T inst = SpringBeanUtils.getInstance().getBeanByClassName(className);
            if (Objects.isNull(inst)) {
                Class<?> clazz = Class.forName(className, false, this);
                //Exclude  ShenyuPlugin subclass and PluginDataHandler subclass
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

    private ShenyuLoaderResult buildResult(final Object instance) {
        ShenyuLoaderResult result = new ShenyuLoaderResult();
        if (instance instanceof ShenyuPlugin) {
            result.setShenyuPlugin((ShenyuPlugin) instance);
        } else if (instance instanceof PluginDataHandler) {
            result.setPluginDataHandler((PluginDataHandler) instance);
        }
        return result;
    }
    
    private String classNameToPath(final String className) {
        return String.join("", className.replace(".", "/"), ".class");
    }
    
    private void definePackageInternal(final String packageName, final Manifest manifest) {
        if (null != getPackage(packageName)) {
            return;
        }
        Attributes attributes = manifest.getMainAttributes();
        String specTitle = attributes.getValue(Attributes.Name.SPECIFICATION_TITLE);
        String specVersion = attributes.getValue(Attributes.Name.SPECIFICATION_VERSION);
        String specVendor = attributes.getValue(Attributes.Name.SPECIFICATION_VENDOR);
        String implTitle = attributes.getValue(Attributes.Name.IMPLEMENTATION_TITLE);
        String implVersion = attributes.getValue(Attributes.Name.IMPLEMENTATION_VERSION);
        String implVendor = attributes.getValue(Attributes.Name.IMPLEMENTATION_VENDOR);
        definePackage(packageName, specTitle, specVersion, specVendor, implTitle, implVersion, implVendor, null);
    }
    
    private boolean ability(final String name) {
        return !names.contains(name);
    }
    
    private static class PluginJar {
        
        private final JarFile jarFile;
        
        private final File sourcePath;
        
        /**
         * Instantiates a new Plugin jar.
         *
         * @param jarFile    the jar file
         * @param sourcePath the source path
         */
        PluginJar(final JarFile jarFile, final File sourcePath) {
            this.jarFile = jarFile;
            this.sourcePath = sourcePath;
        }

        public String absolutePath() {
            return sourcePath.getAbsolutePath();
        }
    }
}
