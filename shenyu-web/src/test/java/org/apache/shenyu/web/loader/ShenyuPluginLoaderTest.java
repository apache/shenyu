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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

import static org.hamcrest.CoreMatchers.is;
import static org.mockito.Mockito.spy;

/**
 * Test for  ShenyuPluginLoader.
 */
public class ShenyuPluginLoaderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private ShenyuPluginLoader shenyuPluginLoader;

    private String path;

    @Before
    public void setUp() throws IOException, NoSuchFieldException, IllegalAccessException {
        shenyuPluginLoader = ShenyuPluginLoader.getInstance();
        File jar = folder.newFile("plugin.jar");
        path = jar.getParent();
        FileOutputStream fos = new FileOutputStream(jar);
        BufferedOutputStream bos = new BufferedOutputStream(fos);
        try (JarOutputStream jos = new JarOutputStream(bos)) {
            String pluginClz = "public class DividePlugin {}";
            jos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.DividePlugin.class"));
            jos.write(pluginClz.getBytes());
            jos.closeEntry();
        }

        Field objectPool = shenyuPluginLoader.getClass().getDeclaredField("objectPool");
        objectPool.setAccessible(true);
        ConcurrentHashMap<String, Object> objectPoolMap = new ConcurrentHashMap<>();
        objectPoolMap.put("org.apache.shenyu.plugin.DividePlugin", new Object());
        objectPool.set(shenyuPluginLoader, objectPoolMap);
    }

    @Test
    public void testGetInstance() {
        Assert.assertThat(shenyuPluginLoader, is(ShenyuPluginLoader.getInstance()));
    }

    @Test
    public void testGetPluginPathWithNoJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        List<ShenyuLoaderResult> pluginList = shenyuPluginLoader.loadExtendPlugins("test");
        Assert.assertThat(pluginList.size(), is(0));
    }

    @Test
    public void testGetPluginPathWithJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        ShenyuPluginLoader loader = spy(shenyuPluginLoader);
        List<ShenyuLoaderResult> pluginList = loader.loadExtendPlugins(path);
        Assert.assertThat(pluginList.size(), is(1));
    }
}
