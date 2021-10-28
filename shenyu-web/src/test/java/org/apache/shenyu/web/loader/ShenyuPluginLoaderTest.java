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
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

/**
 * The TestCase for ShenyuPluginLoader.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(ShenyuPluginLoader.class)
public class ShenyuPluginLoaderTest {

    private ShenyuPluginLoader shenyuPluginLoader;

    @Rule
    private TemporaryFolder tempFolder = new TemporaryFolder();

    private File[] files;

    private String path;

    @Before
    public void setUp() throws IOException {
        shenyuPluginLoader = ShenyuPluginLoader.getInstance();
        File zip = tempFolder.newFile("plugin.zip");
        path = zip.getPath();
        FileOutputStream fos = new FileOutputStream(path);
        BufferedOutputStream bos = new BufferedOutputStream(fos);
        ZipOutputStream zos = new ZipOutputStream(bos);
        try {
            String pluginClas = "public class ApacheDubboPlugin {}";
            zos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.ApacheDubboPlugin.class"));
            zos.write(pluginClas.getBytes());
            zos.closeEntry();
        } finally {
            zos.close();
        }
    }

    /**
     *  test for getInstance.
     */
    @Test
    public void getInstance() {
        assertThat(ShenyuPluginLoader.getInstance()).isEqualTo(shenyuPluginLoader);
    }

    /**
     * test for loadExtendPlugins with no plugin.
     * @throws Exception the test Exception
     */
    @Test
    public void loadExtendPluginsWithEmpty() throws Exception {
        shenyuPluginLoader = PowerMockito.spy(shenyuPluginLoader);
        PowerMockito.doReturn(new File[0]).when(shenyuPluginLoader).listFiles(any());
        Assert.assertEquals(shenyuPluginLoader.loadExtendPlugins("").size(), 0);
    }

    /**
     * test for loadExtendPlugins with  plugins.
     * @throws Exception the test Exception
     */
    @Test
    public void loadExtendPluginsWithJar() throws Exception {
        shenyuPluginLoader = PowerMockito.spy(shenyuPluginLoader);
        PowerMockito.doReturn(null).when(shenyuPluginLoader).getPluginPath(anyString());
        files = new File[]{new File(path)};
        PowerMockito.doReturn(files).when(shenyuPluginLoader).listFiles(null);
        PowerMockito.doReturn(new Object()).when(shenyuPluginLoader, "getOrCreateInstance", anyString());
        Assert.assertEquals(1, shenyuPluginLoader.loadExtendPlugins("").size());
    }

}
