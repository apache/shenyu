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
import java.util.ArrayList;
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
 *
 * <p>The total budget is the bound on every uncompressed entry, {@code manifest.json} included.
 * Sections are therefore allocated only after the manifest that describes them has been sized:
 * the writer plans each section's final content, renders the manifest from that plan, and trims
 * the plan until the manifest plus the sections fit the budget. Trimming changes a section's own
 * summary, so planning repeats until the plan and the manifest agree. A section cut below its
 * collected size is archived as {@link SectionStatus#TRUNCATED}, a section that receives no
 * allowance is {@link SectionStatus#SKIPPED} with an empty entry, and neither is ever reported as
 * {@link SectionStatus#OK}. Every section keeps its deterministic entry name so the manifest stays
 * a complete index of the archive. A budget too small to hold the mandatory manifest is rejected
 * instead of silently producing an archive that exceeds the advertised bound.</p>
 */
public final class DiagnosticBundleWriter {

    private static final String MANIFEST_ENTRY = "manifest.json";

    private static final String SECTION_PREFIX = "sections/";

    private static final String SECTION_SUFFIX = ".json";

    private static final int MAX_NAME_LENGTH = 64;

    private static final String SECTION_SIZE_CAP_REASON = "section size cap";

    private static final String TOTAL_BUDGET_TRUNCATED_REASON = "total budget truncated";

    private static final String TOTAL_BUDGET_EXHAUSTED_REASON = "total budget exhausted";

    private static final byte[] NO_CONTENT = new byte[0];

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
     * @throws IOException           if the in-memory archive cannot be produced
     * @throws IllegalStateException if the total budget cannot hold the mandatory manifest
     */
    public byte[] write(final DiagnosticManifest manifest) throws IOException {
        Plan plan = plan(manifest);
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        Set<String> used = new HashSet<>();
        try (ZipOutputStream zip = new ZipOutputStream(buffer, StandardCharsets.UTF_8)) {
            putEntry(zip, used, MANIFEST_ENTRY, plan.getManifestBytes());
            for (PlannedSection section : plan.getSections()) {
                String entry = SECTION_PREFIX + safeEntryName(section.getSection().getName()) + SECTION_SUFFIX;
                putEntry(zip, used, entry, section.getContent());
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

    private Plan plan(final DiagnosticManifest manifest) {
        List<DiagnosticSection> originals = manifest.getSections();
        int count = originals.size();
        DiagnosticSection[] base = new DiagnosticSection[count];
        byte[][] contents = new byte[count][];
        int[] allocation = new int[count];
        for (int index = 0; index < count; index++) {
            DiagnosticSection section = originals.get(index);
            byte[] content = section.getContent();
            if (content.length > properties.getMaxSectionBytes()) {
                // The cap is a byte count but the content is text: cut on a character boundary so
                // the entry still decodes. A cap below one whole character yields 0 bytes, and a
                // section that can carry no content at all is SKIPPED, not an empty TRUNCATED one.
                int limit = Utf8Boundary.limit(content, properties.getMaxSectionBytes());
                if (limit == 0) {
                    base[index] = DiagnosticSection.of(section.getName(), SectionStatus.SKIPPED,
                            section.getObservedAtEpochMs(), section.getDurationMs(), SECTION_SIZE_CAP_REASON);
                    content = NO_CONTENT;
                } else {
                    content = Arrays.copyOf(content, limit);
                    base[index] = DiagnosticSection.truncated(section.getName(), section.getObservedAtEpochMs(),
                            section.getDurationMs(), content, SECTION_SIZE_CAP_REASON);
                }
            } else {
                base[index] = section;
            }
            contents[index] = content;
            allocation[index] = content.length;
        }
        int budget = properties.getMaxTotalBytes();
        Plan plan = render(manifest, base, contents, allocation);
        // A round does not strictly guarantee progress: a character-boundary cut can leave a byte
        // or two of the requested reduction unachieved, and a section that becomes TRUNCATED or
        // SKIPPED adds its reason text to the manifest, so the next plan can overflow by more than
        // this one did. Termination therefore rests on the pass bound, not on monotone shrinkage.
        // A section changes status at most twice (OK to TRUNCATED to SKIPPED), and each pass either
        // shrinks the allocation or exhausts it, so 2 * count + 2 passes is enough to reach a
        // stable plan or to prove that none fits; the all-zero pass below is the last resort.
        int maxPasses = 2 * count + 2;
        for (int pass = 0; pass < maxPasses && plan.getTotalBytes() > budget; pass++) {
            if (!trim(contents, allocation, plan.getTotalBytes() - budget)) {
                break;
            }
            plan = render(manifest, base, contents, allocation);
        }
        if (plan.getTotalBytes() > budget) {
            plan = renderEmptySections(manifest, base, contents);
        }
        if (plan.getTotalBytes() > budget) {
            throw new IllegalStateException("shenyu.diagnostics.max-total-bytes=" + budget
                    + " cannot hold the mandatory diagnostic manifest of " + plan.getTotalBytes() + " bytes");
        }
        return plan;
    }

    /**
     * Render the plan that carries no section content at all, the smallest archive this manifest
     * can produce. Used both to decide whether the budget is satisfiable and to size the error when
     * it is not.
     */
    private Plan renderEmptySections(final DiagnosticManifest manifest, final DiagnosticSection[] base,
                                     final byte[][] contents) {
        return render(manifest, base, contents, new int[base.length]);
    }

    private Plan render(final DiagnosticManifest manifest, final DiagnosticSection[] base,
                        final byte[][] contents, final int[] allocation) {
        List<PlannedSection> planned = new ArrayList<>(allocation.length);
        List<DiagnosticSection> sections = new ArrayList<>(allocation.length);
        int sectionBytes = 0;
        for (int index = 0; index < allocation.length; index++) {
            PlannedSection section = plannedSection(base[index], contents[index], allocation[index]);
            planned.add(section);
            sections.add(section.getSection());
            sectionBytes += section.getContent().length;
        }
        byte[] manifestBytes = GsonUtils.getGson()
                .toJson(manifest.withSections(sections).toMap())
                .getBytes(StandardCharsets.UTF_8);
        return new Plan(planned, manifestBytes, sectionBytes + manifestBytes.length);
    }

    private PlannedSection plannedSection(final DiagnosticSection base, final byte[] content, final int allocation) {
        if (allocation >= content.length) {
            return new PlannedSection(base, content);
        }
        if (allocation == 0) {
            DiagnosticSection skipped = DiagnosticSection.of(base.getName(), SectionStatus.SKIPPED,
                    base.getObservedAtEpochMs(), base.getDurationMs(), TOTAL_BUDGET_EXHAUSTED_REASON);
            return new PlannedSection(skipped, NO_CONTENT);
        }
        byte[] cut = Arrays.copyOf(content, allocation);
        // A section already cut by maxSectionBytes keeps that fact; the total budget cut on top of
        // it would otherwise hide why the content was short in the first place.
        String reason = SectionStatus.TRUNCATED == base.getStatus()
                ? SECTION_SIZE_CAP_REASON + "; " + TOTAL_BUDGET_TRUNCATED_REASON
                : TOTAL_BUDGET_TRUNCATED_REASON;
        DiagnosticSection truncated = DiagnosticSection.truncated(base.getName(), base.getObservedAtEpochMs(),
                base.getDurationMs(), cut, reason);
        return new PlannedSection(truncated, cut);
    }

    private boolean trim(final byte[][] contents, final int[] allocation, final int overflow) {
        int remaining = overflow;
        for (int index = allocation.length - 1; index >= 0 && remaining > 0; index--) {
            int current = allocation[index];
            if (current == 0) {
                continue;
            }
            int target = Math.max(0, current - remaining);
            if (target > 0) {
                target = Utf8Boundary.limit(contents[index], target);
            }
            remaining -= current - target;
            allocation[index] = target;
        }
        return remaining <= 0;
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

    /**
     * A section as it will be archived, together with the exact bytes written for it.
     */
    private static final class PlannedSection {

        private final DiagnosticSection section;

        private final byte[] content;

        private PlannedSection(final DiagnosticSection section, final byte[] content) {
            this.section = section;
            this.content = content;
        }

        private DiagnosticSection getSection() {
            return section;
        }

        private byte[] getContent() {
            return content;
        }
    }

    /**
     * A complete archive plan: the sections to write, the manifest describing them and the total
     * uncompressed size the two add up to.
     */
    private static final class Plan {

        private final List<PlannedSection> sections;

        private final byte[] manifestBytes;

        private final int totalBytes;

        private Plan(final List<PlannedSection> sections, final byte[] manifestBytes, final int totalBytes) {
            this.sections = sections;
            this.manifestBytes = manifestBytes;
            this.totalBytes = totalBytes;
        }

        private List<PlannedSection> getSections() {
            return sections;
        }

        private byte[] getManifestBytes() {
            return manifestBytes;
        }

        private int getTotalBytes() {
            return totalBytes;
        }
    }
}
