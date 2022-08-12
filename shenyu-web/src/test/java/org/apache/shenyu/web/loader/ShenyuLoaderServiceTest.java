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
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationContext;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doThrow;

/**
 * Test for ShenyuLoaderServiceTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ShenyuLoaderServiceTest {

    @TempDir
    private static Path folder;

    private Path path;

    @BeforeEach
    public void setUp() throws IOException, NoSuchFieldException, IllegalAccessException {
        Path jar = folder.resolve("plugin.jar");
        path = jar.getParent();
        try (FileOutputStream fos = new FileOutputStream(jar.toFile());
             BufferedOutputStream bos = new BufferedOutputStream(fos);
             JarOutputStream jos = new JarOutputStream(bos)) {
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
    public void loaderExtPluginsTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        final ShenyuPluginLoader loader = ShenyuPluginLoader.getInstance();
        final Field field = ShenyuPluginLoader.class.getDeclaredField("jars");
        field.setAccessible(true);
        field.set(loader, Lists.newArrayList());
        final ShenyuWebHandler shenyuWebHandler = mock(ShenyuWebHandler.class);
        final CommonPluginDataSubscriber commonPluginDataSubscriber = mock(CommonPluginDataSubscriber.class);
        final ShenyuConfig.ExtPlugin extPlugin = new ShenyuConfig.ExtPlugin();
        extPlugin.setEnabled(false);
        final ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setExtPlugin(extPlugin);
        new ShenyuLoaderService(shenyuWebHandler, commonPluginDataSubscriber, shenyuConfig);
        extPlugin.setEnabled(true);
        extPlugin.setPath(path.toString());
        ShenyuLoaderService shenyuLoaderService = new ShenyuLoaderService(shenyuWebHandler, commonPluginDataSubscriber, shenyuConfig);

        final Method loaderExtPlugins = ShenyuLoaderService.class.getDeclaredMethod("loaderExtPlugins");
        loaderExtPlugins.setAccessible(true);
        loaderExtPlugins.invoke(shenyuLoaderService);

        doThrow(ShenyuException.class).when(shenyuWebHandler).putExtPlugins(any());
        loaderExtPlugins.invoke(shenyuLoaderService);

        extPlugin.setPath("test");
        loaderExtPlugins.invoke(shenyuLoaderService);
        ShenyuPluginLoader.getInstance().close();
    }
}
