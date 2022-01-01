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

package org.apache.shenyu.agent.core.loader;

import com.google.common.collect.ImmutableMap;
import com.google.common.io.ByteStreams;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint;
import org.apache.shenyu.agent.api.spi.AgentPluginDefinition;
import org.apache.shenyu.agent.core.bytebuddy.matcher.ShenyuAgentTypeMatcher;
import org.apache.shenyu.agent.core.locator.ShenyuAgentLocator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

/**
 * The type Shenyu agent plugin loader.
 */
public final class ShenyuAgentPluginLoader extends ClassLoader implements Closeable {
    
    static {
        registerAsParallelCapable();
    }
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuAgentPluginLoader.class);
    
    private static final ShenyuAgentPluginLoader AGENT_PLUGIN_LOADER = new ShenyuAgentPluginLoader();
    
    private final List<PluginJar> jars = new ArrayList<>();
    
    private ShenyuAgentPluginLoader() {
        super(ShenyuAgentPluginLoader.class.getClassLoader());
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ShenyuAgentPluginLoader getInstance() {
        return AGENT_PLUGIN_LOADER;
    }
    
    /**
     * Load all plugins.
     *
     * @throws IOException IO exception
     */
    public void loadAllPlugins() throws IOException {
        File[] jarFiles = ShenyuAgentLocator.locatorPlugin().listFiles(file -> file.getName().endsWith(".jar"));
        if (Objects.isNull(jarFiles)) {
            return;
        }
        Map<String, ShenyuAgentJoinPoint> pointMap = new HashMap<>();
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            for (File each : jarFiles) {
                outputStream.reset();
                JarFile jar = new JarFile(each, true);
                jars.add(new PluginJar(jar, each));
            }
        }
        loadAgentPluginDefinition(pointMap);
        Map<String, ShenyuAgentJoinPoint> joinPointMap = ImmutableMap.<String, ShenyuAgentJoinPoint>builder().putAll(pointMap).build();
        ShenyuAgentTypeMatcher.getInstance().setJoinPointMap(joinPointMap);
    }
    
    @Override
    protected Class<?> findClass(final String name) throws ClassNotFoundException {
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
                    return defineClass(name, data, 0, data.length);
                } catch (final IOException ex) {
                    LOG.error("Failed to load class {}", name, ex);
                }
            }
        }
        throw new ClassNotFoundException(String.format("Class name is %s not found", name));
    }
    
    @Override
    protected Enumeration<URL> findResources(final String name) {
        Collection<URL> resources = new LinkedList<>();
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
                LOG.error("Exception occur when closing jar", ex);
            }
        }
    }
    
    private void loadAgentPluginDefinition(final Map<String, ShenyuAgentJoinPoint> pointMap) {
        SPILoader.loadList(AgentPluginDefinition.class)
                .forEach(each -> each.collector().forEach(def -> {
                    String classTarget = def.getClassTarget();
                    if (pointMap.containsKey(classTarget)) {
                        ShenyuAgentJoinPoint pluginInterceptorPoint = pointMap.get(classTarget);
                        pluginInterceptorPoint.getConstructorPoints().addAll(def.getConstructorPoints());
                        pluginInterceptorPoint.getInstanceMethodPoints().addAll(def.getInstanceMethodPoints());
                        pluginInterceptorPoint.getStaticMethodPoints().addAll(def.getStaticMethodPoints());
                    } else {
                        pointMap.put(classTarget, def);
                    }
                }));
    }
    
    private String classNameToPath(final String className) {
        return String.join("", className.replace(".", "/"), ".class");
    }
    
    private void definePackageInternal(final String packageName, final Manifest manifest) {
        if (Objects.isNull(getPackage(packageName))) {
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
    
    private static class PluginJar {
        
        private final JarFile jarFile;
        
        private final File sourcePath;
    
        /**
         * Instantiates a new Plugin jar.
         *
         * @param jarFile the jar file
         * @param sourcePath the source path
         */
        PluginJar(final JarFile jarFile, final File sourcePath) {
            this.jarFile = jarFile;
            this.sourcePath = sourcePath;
        }
    }
}
