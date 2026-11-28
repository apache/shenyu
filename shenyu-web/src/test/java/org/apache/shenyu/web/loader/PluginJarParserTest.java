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
import org.apache.shenyu.common.config.PluginArchiveLimits;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.Base64;
import java.util.Arrays;
import java.util.Objects;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link PluginJarParser}.
 */
public final class PluginJarParserTest {

    private static final byte[] METADATA = "groupId=test\nartifactId=plugin\nversion=1\n".getBytes(StandardCharsets.UTF_8);

    @TempDir
    private Path directory;

    @Test
    public void rejectsInvalidConfiguration() {
        PluginArchiveLimits limits = new PluginArchiveLimits();
        assertThrows(IllegalArgumentException.class, () -> limits.setMaxArchiveBytes(0));
        assertThrows(IllegalArgumentException.class, () -> limits.setMaxEntries(-1));
        assertThrows(IllegalArgumentException.class, () -> limits.setMaxEntryBytes(Integer.MAX_VALUE));
        assertThrows(IllegalArgumentException.class, () -> limits.setMaxExpandedBytes(0));
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2})
    public void archiveBoundaryAppliesToBytesFileAndEncodedInput(final int trailingBytes) throws IOException {
        byte[] original = budgetJar("resource", new byte[128]);
        final byte[] archive = Arrays.copyOf(original, original.length + trailingBytes);
        Path path = directory.resolve("plugin.jar");
        Files.write(path, archive);
        final String encoded = Base64.getEncoder().encodeToString(archive);
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxArchiveBytes(archive.length);
        assertEquals("test:plugin", PluginJarParser.parseJar(archive, limits).getJarKey());
        assertEquals("test:plugin", PluginJarParser.parseFile(path, limits).getJarKey());
        assertEquals("test:plugin", PluginJarParser.parseEncodedJar(encoded, limits).getJarKey());
        assertEquals("test:plugin", PluginJarParser.parseEncodedJar(encoded.replace("=", ""), limits).getJarKey());
        limits.setMaxArchiveBytes(archive.length - 1);
        assertTrue(assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(archive, limits)).getMessage().contains("archive input"));
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseFile(path, limits));
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseEncodedJar(encoded, limits));
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseEncodedJar(encoded.replace("=", ""), limits));
    }

    @Test
    public void fileReaderStopsAfterLimitAndClosesRejectedInput() throws IOException {
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxArchiveBytes(8);
        ByteArrayInputStream input = spy(new ByteArrayInputStream(new byte[256]));
        Path path = directory.resolve("oversized.jar");
        try (MockedStatic<Files> files = mockStatic(Files.class)) {
            files.when(() -> Files.newInputStream(path)).thenReturn(input);
            assertThrows(ShenyuException.class, () -> PluginJarParser.parseFile(path, limits));
            assertEquals(247, input.available());
            verify(input).close();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"sample/Test.class", "resource", "META-INF/MANIFEST.MF", "nested.jar", "directory/"})
    public void everyEntryUsesActualExpandedByteBudget(final String name) throws IOException {
        byte[] content = new byte[4096];
        final byte[] archive = budgetJar(name, content);
        assertTrue(archive.length < content.length);
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxEntryBytes(content.length);
        assertEquals("test:plugin", PluginJarParser.parseJar(archive, limits).getJarKey());
        limits.setMaxEntryBytes(content.length - 1);
        assertTrue(assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(archive, limits)).getMessage().contains("expanded entry"));
    }

    @Test
    public void metadataAndEmptyDirectoriesConsumeBudgets() throws IOException {
        final byte[] archive = budgetJar("directory/", new byte[0]);
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxEntryBytes(METADATA.length);
        limits.setMaxExpandedBytes(METADATA.length);
        limits.setMaxEntries(2);
        assertEquals("test:plugin", PluginJarParser.parseJar(archive, limits).getJarKey());
        limits.setMaxEntries(1);
        assertTrue(assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(archive, limits)).getMessage().contains("entry count"));
        limits.setMaxEntries(2);
        limits.setMaxEntryBytes(METADATA.length - 1);
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(archive, limits));
    }

    @Test
    public void totalBudgetIncludesMetadataAndDuplicateNames() throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (JarOutputStream jar = new JarOutputStream(output)) {
            writeEntry(jar, "pom.properties", METADATA);
            writeEntry(jar, "aaa", new byte[100]);
            writeEntry(jar, "bbb", new byte[100]);
        }
        final byte[] archive = output.toByteArray();
        // Rewrite both local and central names to produce legal duplicate ZIP entries.
        for (int index = 0; index < archive.length - 2; index++) {
            if (archive[index] == 'b' && archive[index + 1] == 'b' && archive[index + 2] == 'b') {
                archive[index] = 'a';
                archive[index + 1] = 'a';
                archive[index + 2] = 'a';
            }
        }
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxExpandedBytes(METADATA.length + 200);
        assertEquals(1, PluginJarParser.parseJar(archive, limits).getResourceMap().size());
        limits.setMaxExpandedBytes(METADATA.length + 199);
        assertTrue(assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(archive, limits)).getMessage().contains("expanded total"));
        assertEquals("test:plugin", PluginJarParser.parseJar(budgetJar("valid", new byte[1]), limits).getJarKey());
    }

    @Test
    public void leadingManifestCannotBypassEntryCount() throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (JarOutputStream jar = new JarOutputStream(output)) {
            writeEntry(jar, "META-INF/MANIFEST.MF", new byte[100]);
            writeEntry(jar, "pom.properties", METADATA);
        }
        PluginArchiveLimits limits = new PluginArchiveLimits();
        limits.setMaxEntries(1);
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(output.toByteArray(), limits));
        limits.setMaxEntries(2);
        limits.setMaxEntryBytes(99);
        assertThrows(ShenyuException.class, () -> PluginJarParser.parseJar(output.toByteArray(), limits));
    }

    private byte[] budgetJar(final String name, final byte[] content) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (JarOutputStream jar = new JarOutputStream(output)) {
            writeEntry(jar, "pom.properties", METADATA);
            writeEntry(jar, name, content);
        }
        return output.toByteArray();
    }


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
