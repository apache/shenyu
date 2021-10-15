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

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.AccessController;
import java.security.PrivilegedAction;
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

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuPluginLoaderTest.class);

    private ShenyuPluginLoader shenyuPluginLoader;

    private File[] files;

    private Path path;

    @BeforeClass
    public static void beforeClass() {
    }

    @AfterClass
    public static void afterClass() {
    }

    @Before
    public void setUp() {
        shenyuPluginLoader = ShenyuPluginLoader.getInstance();
        AccessController.doPrivileged(new PrivilegedAction() {
            public Object run() {
                try {
                    path = Files.createTempFile("plugin", ".zip");
                    String pluginClas = "public class ApacheDubboPlugin {}";
                    try (OutputStream os = Files.newOutputStream(path);
                        ZipOutputStream zos = new ZipOutputStream(os)) {
                        ZipEntry e = new ZipEntry("org.apache.shenyu.plugin.ApacheDubboPlugin.class");
                        zos.putNextEntry(e);
                        zos.write(pluginClas.getBytes());
                    }
                } catch (Exception e) {
                    LOG.error(e.getMessage(), e);
                }
                return null;
            }
        });
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
        files = new File[]{path.toFile()};
        PowerMockito.doReturn(files).when(shenyuPluginLoader).listFiles(null);
        PowerMockito.doReturn(new Object()).when(shenyuPluginLoader, "getOrCreateInstance", anyString());
        Assert.assertEquals(1, shenyuPluginLoader.loadExtendPlugins("").size());
    }

}
