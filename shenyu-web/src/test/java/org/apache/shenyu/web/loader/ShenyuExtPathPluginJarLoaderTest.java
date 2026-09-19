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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShenyuExtPathPluginJarLoaderTest {

    @TempDir
    private Path directory;

    @Test
    void shouldReloadJarWhenVersionChangesAtSamePath() throws IOException {
        Path jar = directory.resolve("plugin.jar");
        writePluginJar(jar, "1.0.0");
        List<PluginJarParser.PluginJar> initial = ShenyuExtPathPluginJarLoader.loadExtendPlugins(directory.toString());
        ShenyuPluginClassLoaderHolder holder = ShenyuPluginClassLoaderHolder.getSingleton();
        String jarKey = jar.toFile().getAbsolutePath();
        try {
            holder.replacePluginClassLoader(initial.get(0), classLoader -> { });

            assertEquals(0, ShenyuExtPathPluginJarLoader.loadExtendPlugins(directory.toString()).size());

            writePluginJar(jar, "1.0.1");
            List<PluginJarParser.PluginJar> replacement = ShenyuExtPathPluginJarLoader.loadExtendPlugins(directory.toString());

            assertEquals(1, replacement.size());
            assertEquals("1.0.1", replacement.get(0).getVersion());
            assertTrue(holder.hasPluginClassLoader(jarKey, "1.0.0"));
            assertThrows(IllegalStateException.class,
                    () -> holder.replacePluginClassLoader(replacement.get(0), classLoader -> {
                        throw new IllegalStateException("load failed");
                    }));
            assertTrue(holder.hasPluginClassLoader(jarKey, "1.0.0"));
            assertEquals(1, ShenyuExtPathPluginJarLoader.loadExtendPlugins(directory.toString()).size());
        } finally {
            holder.removePluginClassLoader(jarKey);
        }
    }

    private void writePluginJar(final Path jar, final String version) throws IOException {
        try (OutputStream output = Files.newOutputStream(jar); JarOutputStream jarOutput = new JarOutputStream(output)) {
            jarOutput.putNextEntry(new JarEntry("META-INF/maven/org.apache.shenyu/plugin/pom.properties"));
            Properties properties = new Properties();
            properties.setProperty("groupId", "org.apache.shenyu");
            properties.setProperty("artifactId", "plugin");
            properties.setProperty("version", version);
            properties.store(jarOutput, null);
            jarOutput.closeEntry();
        }
    }
}
