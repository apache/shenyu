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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.context.ApplicationContext;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * Test for  ShenyuPluginLoader.
 */
public class ShenyuPluginLoaderTest {
    
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
            jos.closeEntry();
        }
        ApplicationContext mockApplication =
                mock(ApplicationContext.class);
        when(mockApplication.getBean("dividePlugin")).thenReturn(new Object());
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
    public void testGetPluginPathWithNoJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        List<ShenyuLoaderResult> pluginList = shenyuPluginLoader.loadExtendPlugins("test");
        assertThat(pluginList.size(), is(0));
    }
    
    @Test
    public void testGetPluginPathWithJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        ShenyuPluginLoader loader = spy(shenyuPluginLoader);
        List<ShenyuLoaderResult> pluginList = loader.loadExtendPlugins(path.toString());
        assertThat(pluginList.size(), is(1));
        loader.close();
    }
}
