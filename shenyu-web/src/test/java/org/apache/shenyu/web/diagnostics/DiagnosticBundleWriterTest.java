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

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

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
import static org.junit.jupiter.api.Assertions.assertThrows;
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

    @Test
    public void manifestDescribesTheArchivedContentAfterTotalBudgetTruncation() throws IOException {
        String payload = "x".repeat(400);
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("runtime", 1L, 1L, payload),
                DiagnosticSection.ok("plugins", 1L, 1L, payload));
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        int completeBytes = uncompressedBytes(read(writer.write(manifest)));
        properties.setMaxTotalBytes(completeBytes - 50);
        Map<String, String> entries = read(writer.write(manifest));
        assertTrue(uncompressedBytes(entries) <= properties.getMaxTotalBytes(),
                "uncompressed entries including the manifest must stay within maxTotalBytes");
        assertTrue(entries.get("manifest.json").contains("\"status\":\"TRUNCATED\""));
        assertManifestMatchesEntries(entries.get("manifest.json"), entries, payload);
    }

    @Test
    public void exhaustedTotalBudgetSkipsSectionsInsteadOfReportingOk() throws IOException {
        DiagnosticSection[] sections = new DiagnosticSection[8];
        for (int index = 0; index < sections.length; index++) {
            sections[index] = DiagnosticSection.ok("section-" + index, 1L, 1L, "z".repeat(400));
        }
        DiagnosticManifest manifest = manifest(sections);
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        int completeBytes = uncompressedBytes(read(writer.write(manifest)));
        // Half the complete bundle still leaves room for the manifest but not for every section.
        properties.setMaxTotalBytes(completeBytes / 2);
        Map<String, String> entries = read(writer.write(manifest));
        String manifestJson = entries.get("manifest.json");
        assertTrue(manifestJson.contains("\"status\":\"SKIPPED\""));
        assertTrue(manifestJson.contains("total budget exhausted"));
        assertTrue(uncompressedBytes(entries) <= properties.getMaxTotalBytes());
        for (JsonElement element : JsonParser.parseString(manifestJson).getAsJsonObject().getAsJsonArray("sections")) {
            JsonObject summary = element.getAsJsonObject();
            if (SectionStatus.SKIPPED.name().equals(summary.get("status").getAsString())) {
                assertEquals(0, summary.get("sizeBytes").getAsInt());
                assertEquals("", entries.get("sections/" + summary.get("name").getAsString() + ".json"));
            }
        }
        assertManifestMatchesEntries(manifestJson, entries, "z".repeat(400));
    }

    @Test
    public void multibyteContentIsCutOnACharacterBoundary() throws IOException {
        String payload = "中文字".repeat(50);
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("runtime", 1L, 1L, payload),
                DiagnosticSection.ok("plugins", 1L, 1L, payload));
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        properties.setMaxTotalBytes(uncompressedBytes(read(writer.write(manifest))) - 1);
        Map<String, String> entries = read(writer.write(manifest));
        assertManifestMatchesEntries(entries.get("manifest.json"), entries, payload);
        assertTrue(entries.get("manifest.json").contains("\"status\":\"TRUNCATED\""));
        for (Map.Entry<String, String> entry : entries.entrySet()) {
            if (entry.getKey().startsWith("sections/")) {
                assertFalse(entry.getValue().contains("�"), "a cut must not split a UTF-8 sequence");
                assertTrue(payload.startsWith(entry.getValue()));
            }
        }
    }

    @Test
    public void completeBundlesAreUnaffectedByTheTotalBudget() throws IOException {
        DiagnosticManifest manifest = manifest(
                DiagnosticSection.ok("runtime", 1L, 1L, "{\"a\":1}"),
                DiagnosticSection.ok("plugins", 1L, 1L, "[]"));
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        properties.setMaxTotalBytes(uncompressedBytes(read(writer.write(manifest))));
        Map<String, String> entries = read(writer.write(manifest));
        assertTrue(entries.get("manifest.json").contains("\"status\":\"OK\""));
        assertEquals("{\"a\":1}", entries.get("sections/runtime.json"));
        assertEquals("[]", entries.get("sections/plugins.json"));
    }

    @Test
    public void budgetSmallerThanTheManifestFailsExplicitly() {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxTotalBytes(16);
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        IllegalStateException ex = assertThrows(IllegalStateException.class,
                () -> writer.write(manifest(DiagnosticSection.ok("runtime", 1L, 1L, "{}"))));
        assertTrue(ex.getMessage().contains("max-total-bytes=16"));
    }

    /**
     * A per-section cap that lands inside a 3-byte character must back off to the character start,
     * never emit a half-encoded sequence, and report SKIPPED when no whole character fits.
     *
     * @param capBytes the configured maxSectionBytes
     * @param expected the number of bytes the entry is expected to keep
     * @param status   the expected status of the archived section
     */
    @ParameterizedTest
    @CsvSource({
        "1, 0, SKIPPED",
        "2, 0, SKIPPED",
        "3, 3, TRUNCATED",
        "4, 3, TRUNCATED",
        "5, 3, TRUNCATED",
        "6, 6, OK"
    })
    public void sectionCapNeverSplitsAMultibyteCharacter(final int capBytes, final int expected,
                                                         final SectionStatus status) throws IOException {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxSectionBytes(capBytes);
        DiagnosticBundleWriter writer = new DiagnosticBundleWriter(properties);
        Map<String, String> entries = read(writer.write(manifest(
                DiagnosticSection.ok("runtime", 1L, 1L, "中文"))));
        String content = entries.get("sections/runtime.json");
        assertEquals(expected, content.getBytes(StandardCharsets.UTF_8).length);
        assertEquals("中文".substring(0, expected / 3), content);
        JsonObject summary = JsonParser.parseString(entries.get("manifest.json")).getAsJsonObject()
                .getAsJsonArray("sections").get(0).getAsJsonObject();
        assertEquals(status.name(), summary.get("status").getAsString());
        assertEquals(expected, summary.get("sizeBytes").getAsInt());
    }

    /**
     * The same boundary rule applies to the per-section cap the collector applies before archiving.
     */
    @Test
    public void collectedSectionCapNeverSplitsAMultibyteCharacter() {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxSectionBytes(4);
        DiagnosticBundleService service = new DiagnosticBundleService(
                List.of(new FixedCollector("runtime", "中文")), properties, new DiagnosticBundleWriter(properties));
        DiagnosticSection section = service.collect().getSections().get(0);
        assertEquals(SectionStatus.TRUNCATED, section.getStatus());
        assertEquals("中", new String(section.getContent(), StandardCharsets.UTF_8));
    }

    private int uncompressedBytes(final Map<String, String> entries) {
        int total = 0;
        for (String content : entries.values()) {
            total += content.getBytes(StandardCharsets.UTF_8).length;
        }
        return total;
    }

    /**
     * Assert the manifest is an accurate index of the archive and never calls a shortened section OK.
     */
    private void assertManifestMatchesEntries(final String manifestJson, final Map<String, String> entries,
                                              final String collectedPayload) {
        JsonObject manifest = JsonParser.parseString(manifestJson).getAsJsonObject();
        for (JsonElement element : manifest.getAsJsonArray("sections")) {
            JsonObject summary = element.getAsJsonObject();
            String name = summary.get("name").getAsString();
            String content = entries.get("sections/" + name + ".json");
            assertEquals(content.getBytes(StandardCharsets.UTF_8).length, summary.get("sizeBytes").getAsInt(),
                    "sizeBytes must describe the archived entry for " + name);
            SectionStatus status = SectionStatus.valueOf(summary.get("status").getAsString());
            if (SectionStatus.OK == status) {
                assertEquals(collectedPayload.length(), content.length(), "an OK section must be archived in full");
            } else {
                assertTrue(SectionStatus.TRUNCATED == status || SectionStatus.SKIPPED == status,
                        "a section shortened by the total budget must not be reported as " + status);
                assertTrue(Objects.nonNull(summary.get("reason")));
            }
        }
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

    private static final class FixedCollector implements SectionCollector {

        private final String name;

        private final String content;

        FixedCollector(final String name, final String content) {
            this.name = name;
            this.content = content;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public DiagnosticSection collect(final DiagnosticBudget budget) {
            return DiagnosticSection.ok(name, System.currentTimeMillis(), 0L, content);
        }
    }
}
