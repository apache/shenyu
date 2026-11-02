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

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link PluginJarParser}.
 */
public final class PluginJarParserTest {

    @Test
    public void testParseJarWithMavenMetadata() throws IOException {
        PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(createJar("groupId=org.apache.shenyu\nartifactId=test-plugin\nversion=1.0.0\n"));
        assertEquals("org.apache.shenyu:test-plugin", pluginJar.getJarKey());
        assertEquals("1.0.0", pluginJar.getVersion());
    }

    @Test
    public void testParseJarWithoutMavenMetadata() throws IOException {
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(createJar(null)));
    }

    @Test
    public void testParseJarWithIncompleteMavenMetadata() throws IOException {
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(createJar("artifactId=test-plugin\nversion=1.0.0\n")));
    }

    private byte[] createJar(final String pomProperties) throws IOException {
        try (ByteArrayOutputStream output = new ByteArrayOutputStream();
             JarOutputStream jar = new JarOutputStream(output)) {
            jar.putNextEntry(new JarEntry("TestPlugin.class"));
            jar.write(new byte[] {0});
            jar.closeEntry();
            if (Objects.nonNull(pomProperties)) {
                jar.putNextEntry(new JarEntry("META-INF/maven/org.apache.shenyu/test-plugin/pom.properties"));
                jar.write(pomProperties.getBytes(StandardCharsets.UTF_8));
                jar.closeEntry();
            }
            jar.finish();
            return output.toByteArray();
        }
    }

    @Test
    public void shouldStoreInnerClassesInClassMap() throws IOException {
        final byte[] outerClass = {1, 2, 3};
        final byte[] innerClass = {4, 5, 6};
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (JarOutputStream jarOutput = new JarOutputStream(output)) {
            writeEntry(jarOutput, "META-INF/maven/test/plugin/pom.properties",
                    "groupId=test\nartifactId=plugin\nversion=1\n".getBytes(StandardCharsets.UTF_8));
            writeEntry(jarOutput, "sample/Outer.class", outerClass);
            writeEntry(jarOutput, "sample/Outer$Inner.class", innerClass);
        }

        PluginJarParser.PluginJar pluginJar = PluginJarParser.parseJar(output.toByteArray());

        assertArrayEquals(outerClass, pluginJar.getClazzMap().get("sample.Outer"));
        assertArrayEquals(innerClass, pluginJar.getClazzMap().get("sample.Outer$Inner"));
        assertTrue(pluginJar.getResourceMap().isEmpty());
    }

    private void writeEntry(final JarOutputStream jarOutput, final String name, final byte[] content) throws IOException {
        jarOutput.putNextEntry(new JarEntry(name));
        jarOutput.write(content);
        jarOutput.closeEntry();
    }
}
