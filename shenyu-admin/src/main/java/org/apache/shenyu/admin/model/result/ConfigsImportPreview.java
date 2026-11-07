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

package org.apache.shenyu.admin.model.result;

import org.apache.shenyu.admin.model.enums.ConfigImportAction;

import java.util.List;

/**
 * Read-only plan for a namespace configuration import.
 *
 * @param observedAt observation time in ISO-8601 form
 * @param archiveDigest SHA-256 digest of the uploaded archive
 * @param baselineFingerprint SHA-256 fingerprint of target identity rows
 * @param previewToken optional token binding this preview to its archive and baseline
 * @param targetNamespace preview target namespace
 * @param fullyValidated whether every present section was analyzed without coverage gaps
 * @param truncated whether the outcome budget stopped the plan
 * @param summary outcome counts
 * @param sections per-section plans
 * @param warnings bounded, machine-readable warnings
 */
public record ConfigsImportPreview(
        String observedAt,
        String archiveDigest,
        String baselineFingerprint,
        String previewToken,
        String targetNamespace,
        boolean fullyValidated,
        boolean truncated,
        Summary summary,
        List<SectionPreview> sections,
        List<ConfigsImportWarning> warnings) {

    /**
     * Defensive copy of sections and warnings.
     *
     * @param observedAt observation time
     * @param archiveDigest archive digest
     * @param baselineFingerprint baseline fingerprint
     * @param previewToken optional preview token
     * @param targetNamespace target namespace
     * @param fullyValidated whether every present section was analyzed
     * @param truncated whether the report was truncated
     * @param summary outcome counts
     * @param sections per-section plans
     * @param warnings warnings
     */
    public ConfigsImportPreview {
        sections = List.copyOf(sections);
        warnings = List.copyOf(warnings);
    }

    /**
     * Outcome counts.
     *
     * @param create number of creates
     * @param reuse number of reuses
     * @param skip number of skips
     * @param reject number of rejects
     */
    public record Summary(int create, int reuse, int skip, int reject) {
    }

    /**
     * Plan for one archive section.
     *
     * @param section archive entry name
     * @param scope NAMESPACE or GLOBAL
     * @param supported whether the section was analyzed
     * @param present whether the archive contains the section
     * @param blocking whether the live import would throw and roll back the section
     * @param validatedBy validators or resolution checks applied to the section
     * @param outcomes per-object outcomes
     * @param diagnostics section-level diagnostic codes
     */
    public record SectionPreview(
            String section,
            String scope,
            boolean supported,
            boolean present,
            boolean blocking,
            List<String> validatedBy,
            List<PreviewOutcome> outcomes,
            List<String> diagnostics) {

        /**
         * Defensive copy of the lists.
         *
         * @param section archive entry name
         * @param scope section scope
         * @param supported whether the section was analyzed
         * @param present whether the section is present
         * @param blocking whether the section would abort import
         * @param validatedBy validators or resolution checks applied
         * @param outcomes per-object outcomes
         * @param diagnostics section diagnostics
         */
        public SectionPreview {
            validatedBy = List.copyOf(validatedBy);
            outcomes = List.copyOf(outcomes);
            diagnostics = List.copyOf(diagnostics);
        }
    }

    /**
     * One source object outcome.
     *
     * @param sourceRef report-safe source reference
     * @param sourceName source business name
     * @param targetRef existing with id, existing, new, planned or none
     * @param action CREATE, REUSE, SKIP or REJECT
     * @param remappedRefs bounded reference remapping descriptions
     * @param diagnostics bounded diagnostic codes
     */
    public record PreviewOutcome(
            String sourceRef,
            String sourceName,
            String targetRef,
            ConfigImportAction action,
            List<String> remappedRefs,
            List<String> diagnostics) {

        /**
         * Defensive copy of the lists.
         *
         * @param sourceRef source reference
         * @param sourceName source name
         * @param targetRef target reference
         * @param action preview action
         * @param remappedRefs remapping descriptions
         * @param diagnostics diagnostic codes
         */
        public PreviewOutcome {
            remappedRefs = List.copyOf(remappedRefs);
            diagnostics = List.copyOf(diagnostics);
        }
    }
}
