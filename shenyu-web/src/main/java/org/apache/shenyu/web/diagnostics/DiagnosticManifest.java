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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The bundle manifest describing provenance and the per-section outcome.
 *
 * <p>The manifest is explicit that a bundle is not an atomic cross-component snapshot: every
 * section records its own observation time and status, and the manifest records the source
 * instance boot identity and software versions so a consumer can reason about skew.</p>
 */
public final class DiagnosticManifest {

    /**
     * The bundle format version, bumped on any structural change to the archive layout.
     */
    public static final int FORMAT_VERSION = 1;

    private final long generatedAtEpochMs;

    private final Map<String, Object> instance;

    private final Map<String, String> softwareVersions;

    private final List<DiagnosticSection> sections;

    /**
     * Instantiates a new manifest.
     *
     * @param generatedAtEpochMs the bundle generation timestamp in epoch milliseconds
     * @param instance           the source instance identity
     * @param softwareVersions   the observed software versions
     * @param sections           the collected sections
     */
    public DiagnosticManifest(final long generatedAtEpochMs, final Map<String, Object> instance,
                              final Map<String, String> softwareVersions, final List<DiagnosticSection> sections) {
        this.generatedAtEpochMs = generatedAtEpochMs;
        this.instance = new LinkedHashMap<>(instance);
        this.softwareVersions = new LinkedHashMap<>(softwareVersions);
        this.sections = new ArrayList<>(sections);
    }

    /**
     * Render the manifest as an ordered map suitable for JSON serialisation.
     *
     * @return the manifest map
     */
    public Map<String, Object> toMap() {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("formatVersion", FORMAT_VERSION);
        map.put("generatedAtEpochMs", generatedAtEpochMs);
        map.put("instance", instance);
        map.put("softwareVersions", softwareVersions);
        map.put("atomicSnapshot", Boolean.FALSE);
        List<Map<String, Object>> summaries = new ArrayList<>(sections.size());
        for (DiagnosticSection section : sections) {
            summaries.add(summarize(section));
        }
        map.put("sections", summaries);
        return map;
    }

    /**
     * Get the collected sections.
     *
     * @return the sections
     */
    public List<DiagnosticSection> getSections() {
        return new ArrayList<>(sections);
    }

    private Map<String, Object> summarize(final DiagnosticSection section) {
        Map<String, Object> summary = new LinkedHashMap<>();
        summary.put("name", section.getName());
        summary.put("status", section.getStatus().name());
        summary.put("observedAtEpochMs", section.getObservedAtEpochMs());
        summary.put("durationMs", section.getDurationMs());
        summary.put("sizeBytes", section.getSizeBytes());
        if (Objects.nonNull(section.getReason())) {
            summary.put("reason", section.getReason());
        }
        return summary;
    }
}
