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

import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;
import org.apache.shenyu.spi.SPI;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class ShenyuPluginClassLoaderTest {

    @Test
    void shouldDiscoverAndEvictPluginExtensions() {
        PluginJarParser.PluginJar pluginJar = new PluginJarParser.PluginJar();
        String resourceName = "META-INF/shenyu/" + TestSPI.class.getName();
        String extension = "plugin=" + TestExtension.class.getName();
        pluginJar.setResourceMap(Collections.singletonMap(resourceName, extension.getBytes(StandardCharsets.UTF_8)));
        ShenyuPluginClassLoader classLoader = new ShenyuPluginClassLoader(pluginJar);

        ExtensionLoader<TestSPI> firstLoader = ExtensionLoader.getExtensionLoader(TestSPI.class, classLoader);
        assertEquals(TestExtension.class, firstLoader.getJoin("plugin").getClass());

        classLoader.close();
        ExtensionLoader<TestSPI> secondLoader = ExtensionLoader.getExtensionLoader(TestSPI.class, classLoader);
        assertNotSame(firstLoader, secondLoader);
    }

    @SPI
    private interface TestSPI {
    }

    @Join
    public static class TestExtension implements TestSPI {
    }
}
