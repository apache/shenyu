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

package org.apache.shenyu.spi;

import org.apache.shenyu.spi.fixture.JdbcSPI;
import org.apache.shenyu.spi.fixture.MysqlSPI;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

class ExtensionLoaderClassLoaderTest {

    @TempDir
    private Path directory;

    @Test
    void shouldDiscoverExtensionsFromEachClassLoader() throws IOException {
        Path serviceFile = directory.resolve("META-INF/shenyu/" + JdbcSPI.class.getName());
        Files.createDirectories(serviceFile.getParent());
        Files.write(serviceFile, ("plugin=" + MysqlSPI.class.getName()).getBytes(StandardCharsets.UTF_8));
        ClassLoader applicationClassLoader = ExtensionLoaderClassLoaderTest.class.getClassLoader();

        try (URLClassLoader pluginClassLoader = new URLClassLoader(new java.net.URL[]{directory.toUri().toURL()}, applicationClassLoader)) {
            ExtensionLoader<JdbcSPI> applicationLoader = ExtensionLoader.getExtensionLoader(JdbcSPI.class, applicationClassLoader);
            ExtensionLoader<JdbcSPI> pluginLoader = ExtensionLoader.getExtensionLoader(JdbcSPI.class, pluginClassLoader);

            assertNotSame(applicationLoader, pluginLoader);
            assertEquals(MysqlSPI.class, pluginLoader.getJoin("plugin").getClass());
        }
    }
}
