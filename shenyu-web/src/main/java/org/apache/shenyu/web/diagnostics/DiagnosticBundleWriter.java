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

import org.apache.shenyu.common.utils.GsonUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Packages a manifest and its sections into an in-memory ZIP archive.
 *
 * <p>Entry names are sanitised so a hostile section name cannot introduce path traversal or an
 * absolute path, and both per-entry and total sizes are hard-capped so a bundle can never grow
 * without bound. The archive is built entirely in memory and returned as a byte array; no
 * temporary file touches the disk on this path.</p>
 */
public final class DiagnosticBundleWriter {

    private static final String MANIFEST_ENTRY = "manifest.json";

    private static final String SECTION_PREFIX = "sections/";

    private static final String SECTION_SUFFIX = ".json";

    private static final int MAX_NAME_LENGTH = 64;

    private final DiagnosticProperties properties;

    /**
     * Instantiates a new writer.
     *
     * @param properties the diagnostic bounds
     */
    public DiagnosticBundleWriter(final DiagnosticProperties properties) {
        this.properties = properties;
    }

    /**
     * Write the bundle archive.
     *
     * @param manifest the manifest, which also carries the sections
     * @return the archive bytes
     * @throws IOException if the in-memory archive cannot be produced
     */
    public byte[] write(final DiagnosticManifest manifest) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        Set<String> used = new HashSet<>();
        try (ZipOutputStream zip = new ZipOutputStream(buffer, StandardCharsets.UTF_8)) {
            putEntry(zip, used, MANIFEST_ENTRY, GsonUtils.getGson().toJson(manifest.toMap()).getBytes(StandardCharsets.UTF_8));
            List<DiagnosticSection> sections = manifest.getSections();
            int remaining = properties.getMaxTotalBytes();
            for (DiagnosticSection section : sections) {
                byte[] content = cap(section.getContent(), properties.getMaxSectionBytes());
                if (content.length > remaining) {
                    content = Arrays.copyOf(content, Math.max(0, remaining));
                }
                remaining -= content.length;
                putEntry(zip, used, SECTION_PREFIX + safeEntryName(section.getName()) + SECTION_SUFFIX, content);
            }
        }
        return buffer.toByteArray();
    }

    /**
     * Sanitise a section name into a single, relative, traversal-safe archive entry component.
     *
     * @param name the raw section name
     * @return a safe entry component
     */
    public static String safeEntryName(final String name) {
        if (Objects.isNull(name)) {
            return "section";
        }
        String cleaned = name.replaceAll("[^A-Za-z0-9._-]", "_");
        cleaned = cleaned.replaceAll("\\.{2,}", ".");
        while (cleaned.startsWith(".")) {
            cleaned = cleaned.substring(1);
        }
        if (cleaned.length() > MAX_NAME_LENGTH) {
            cleaned = cleaned.substring(0, MAX_NAME_LENGTH);
        }
        if (cleaned.isEmpty() || ".".equals(cleaned) || "..".equals(cleaned)) {
            return "section";
        }
        return cleaned;
    }

    private void putEntry(final ZipOutputStream zip, final Set<String> used, final String name, final byte[] content) throws IOException {
        String unique = name;
        int suffix = 1;
        while (!used.add(unique)) {
            unique = name + "-" + suffix++;
        }
        zip.putNextEntry(new ZipEntry(unique));
        zip.write(content);
        zip.closeEntry();
    }

    private byte[] cap(final byte[] content, final int maxBytes) {
        if (content.length <= maxBytes) {
            return content;
        }
        return Arrays.copyOf(content, maxBytes);
    }
}
