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

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for  ShenyuPluginLoader.
 */
public final class ShenyuPluginLoaderTest {

    @TempDir
    private static Path folder;
    
    private ShenyuPluginLoader shenyuPluginLoader;
    
    private Path path;
    
    @BeforeEach
    public void setUp() throws IOException, NoSuchFieldException, IllegalAccessException {
        shenyuPluginLoader = ShenyuPluginLoader.getInstance();
        Path jar = folder.resolve("plugin.jar");
        path = jar.getParent();
        FileOutputStream fos = new FileOutputStream(jar.toFile());
        BufferedOutputStream bos = new BufferedOutputStream(fos);
        try (JarOutputStream jos = new JarOutputStream(bos)) {
            String pluginClz = "public class DividePlugin {}";
            jos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.DividePlugin.class"));
            jos.write(pluginClz.getBytes());
            jos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.Plugin.class"));
            jos.write(pluginClz.getBytes());
            jos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.DividePlugin"));
            jos.write(pluginClz.getBytes());
            jos.putNextEntry(new ZipEntry("org/apache/shenyu/plugin/DividePlugin.class"));
            jos.write(pluginClz.getBytes());
            String MANIFEST = "Manifest-Version: 1.0\n" +
                    "Ant-Version: Apache Ant 1.9.3\n" +
                    "Created-By: 1.6.0_65-b14-462-11M4609 (Apple Inc.)\n" +
                    "Main-Class: org.apache.tools.ant.Main\n" +
                    "\n" +
                    "Name: org/apache/tools/ant/\n" +
                    "Extension-name: org.apache.tools.ant\n" +
                    "Specification-Title: Apache Ant\n" +
                    "Specification-Version: 1.9.3\n" +
                    "Specification-Vendor: Apache Software Foundation\n" +
                    "Implementation-Title: org.apache.tools.ant\n" +
                    "Implementation-Version: 1.9.3\n" +
                    "Implementation-Vendor: Apache Software Foundation\n";
            jos.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"));
            jos.write(MANIFEST.getBytes());
            jos.closeEntry();
        }
        ApplicationContext mockApplication =
                mock(ApplicationContext.class);
        when(mockApplication.getBean("dividePlugin")).thenReturn(new Object());
        when(mockApplication.getAutowireCapableBeanFactory()).thenReturn(mock(DefaultListableBeanFactory.class));
        when(mockApplication.containsBean("dividePlugin")).thenReturn(true);
        SpringBeanUtils instance = SpringBeanUtils.getInstance();
        instance.setApplicationContext(mockApplication);
    }
    
    @Test
    public void testGetBean() {
        boolean exist = SpringBeanUtils.getInstance().existBean("dividePlugin");
        assertTrue(exist);
        Object dividePlugin = SpringBeanUtils.getInstance().getBean("dividePlugin");
        assertNotNull(dividePlugin);
    }
    
    @Test
    public void testGetInstance() {
        assertThat(shenyuPluginLoader, is(ShenyuPluginLoader.getInstance()));
    }
    
    @Test
    public void testGetPluginPathWithNoJar() throws IOException {
        List<ShenyuLoaderResult> pluginList = shenyuPluginLoader.loadExtendPlugins("test");
        assertThat(pluginList.size(), is(0));
    }
    
    @Test
    public void testGetPluginPathWithJar() throws IOException {
        ShenyuPluginLoader loader = ShenyuPluginLoader.getInstance();
        List<ShenyuLoaderResult> pluginList = loader.loadExtendPlugins(path.toString());
        assertThat(pluginList.size(), is(1));
        Assertions.assertTrue(loader.findResources("").hasMoreElements());
        loader.findResources("org.apache.shenyu.plugin.DividePlugin");
        Assertions.assertNotNull(loader.findResource("org.apache.shenyu.plugin.DividePlugin"));
        Assertions.assertNull(loader.findResource("DividePlugin"));
        Assertions.assertNull(loader.findResource("org.apache.shenyu.plugin.Plugin"));
        loader.close();
    }

    @Test
    public void testGetOrCreateSpringBean() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method getOrCreateSpringBean = ShenyuPluginLoader.class.getDeclaredMethod("getOrCreateSpringBean", String.class);
        getOrCreateSpringBean.setAccessible(true);
        getOrCreateSpringBean.invoke(ShenyuPluginLoader.getInstance(), "org.apache.shenyu.web.loader.ShenyuPluginLoaderTest$TestComponent");
        getOrCreateSpringBean.invoke(ShenyuPluginLoader.getInstance(), "org.apache.shenyu.web.loader.ShenyuPluginLoaderTest$TestService");
    }

    @Test
    public void testFindClass() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
        ShenyuPluginLoader loader = ShenyuPluginLoader.getInstance();
        loader.loadExtendPlugins(path.toString());
        Method findClass = ShenyuPluginLoader.class.getDeclaredMethod("findClass", String.class);
        findClass.setAccessible(true);
        findClass.invoke(loader, "org.apache.shenyu.plugin.DividePlugin");
        loader.close();
    }

    @Test
    public void testDefinePackageInternal() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IOException {
        Method definePackageInternal = ShenyuPluginLoader.class.getDeclaredMethod("definePackageInternal", String.class, Manifest.class);
        definePackageInternal.setAccessible(true);
        Manifest manifest = mock(Manifest.class);
        when(manifest.getMainAttributes()).thenReturn(mock(Attributes.class));
        definePackageInternal.invoke(ShenyuPluginLoader.getInstance(), "org.apache.shenyu.plugin.DividePlugin", manifest);
    }

    @Component
    public static class TestComponent {
    }
    @Service
    public static class TestService {
    }
}
