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

package org.apache.shenyu.web.diagnostics;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link DiagnosticBundleWriter}.
 */
public final class DiagnosticBundleWriterTest {

    @Test
    public void archiveContainsManifestAndSections() throws IOException {
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(new DiagnosticProperties());
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("runtime", 1L, 1L, "{\"a\":1}"),
                DiagnosticSection.ok("plugins", 1L, 1L, "[]"));
        Map<String, String> entries = read(writer.write(manifest));
        assertTrue(entries.containsKey("manifest.json"));
        assertTrue(entries.containsKey("sections/runtime.json"));
        assertTrue(entries.containsKey("sections/plugins.json"));
        assertTrue(entries.get("manifest.json").contains("formatVersion"));
    }

    @Test
    public void entryNamesAreTraversalSafe() throws IOException {
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(new DiagnosticProperties());
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("../../etc/passwd", 1L, 1L, "x"),
                DiagnosticSection.ok("/absolute/win\\path", 1L, 1L, "y"));
        Map<String, String> entries = read(writer.write(manifest));
        for (String name : entries.keySet()) {
            if (name.startsWith("sections/")) {
                String component = name.substring("sections/".length(), name.length() - ".json".length());
                assertFalse(component.contains(".."));
                assertFalse(component.contains("/"));
                assertFalse(component.contains("\\"));
                assertFalse(component.isEmpty());
            }
        }
        assertEquals(3, entries.size());
    }

    @Test
    public void safeEntryNameNeutralisesHostileNames() {
        assertEquals("section", DiagnosticBundleWriter.safeEntryName(".."));
        assertEquals("section", DiagnosticBundleWriter.safeEntryName(""));
        assertEquals("section", DiagnosticBundleWriter.safeEntryName(null));
        assertFalse(DiagnosticBundleWriter.safeEntryName("../../x").contains(".."));
        assertEquals("a_b", DiagnosticBundleWriter.safeEntryName("a/b"));
    }

    @Test
    public void perEntrySizeIsCapped() throws IOException {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxSectionBytes(16);
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        String big = "x".repeat(200);
        DiagnosticManifest manifest = manifest(DiagnosticSection.ok("runtime", 1L, 1L, big));
        Map<String, String> entries = read(writer.write(manifest));
        assertTrue(entries.get("sections/runtime.json").length() <= 16);
    }

    @Test
    public void duplicateSectionNamesProduceUniqueEntries() throws IOException {
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(new DiagnosticProperties());
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("dup", 1L, 1L, "a"),
                DiagnosticSection.ok("dup", 1L, 1L, "b"));
        Map<String, String> entries = read(writer.write(manifest));
        assertTrue(entries.containsKey("sections/dup.json"));
        assertEquals(3, entries.size());
    }

    private DiagnosticManifest manifest(final DiagnosticSection... sections) {
        return new DiagnosticManifest(System.currentTimeMillis(), Map.of("hostAddress", "127.0.0.1"),
                Map.of("shenyu", "test"), List.of(sections));
    }

    private Map<String, String> read(final byte[] archive) throws IOException {
        Map<String, String> entries = new LinkedHashMap<>();
        try (ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(archive), StandardCharsets.UTF_8)) {
            ZipEntry entry = zip.getNextEntry();
            while (Objects.nonNull(entry)) {
                entries.put(entry.getName(), new String(zip.readAllBytes(), StandardCharsets.UTF_8));
                entry = zip.getNextEntry();
            }
        }
        return entries;
    }
}
