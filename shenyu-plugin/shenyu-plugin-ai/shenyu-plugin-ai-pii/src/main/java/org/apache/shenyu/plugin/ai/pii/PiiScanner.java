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

package org.apache.shenyu.plugin.ai.pii;

import org.apache.shenyu.plugin.ai.pii.audit.PiiAuditRecord;
import org.apache.shenyu.plugin.ai.pii.detector.PiiDetector;
import org.apache.shenyu.plugin.ai.pii.handler.PiiDetectionRule;
import org.apache.shenyu.plugin.ai.pii.profile.PiiTextSlot;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The scan of one direction of a payload against the compiled rule. The scan
 * applies the strictest action of the rule: a single block level detection
 * blocks the whole direction, otherwise redactions are applied to the slots
 * and every redaction or audit level observation is reported as an audit
 * record without the detected value.
 *
 * <p>Overlapping detections on the same slot are resolved by position and
 * strictness: ranges are replaced from the end of the text backwards, and a
 * range that overlaps an already replaced range is skipped unless its action
 * is stricter, in which case the stricter replacement wins because the sort
 * places it first.</p>
 */
final class PiiScanner {

    private final PiiDetectionRule rule;

    private final String direction;

    PiiScanner(final PiiDetectionRule rule, final String direction) {
        this.rule = rule;
        this.direction = direction;
    }

    /**
     * Scan the slots of one direction.
     *
     * @param slots the text slots of the payload
     * @return the outcome of the scan
     */
    PiiOutcome scan(final List<PiiTextSlot> slots) {
        // one deadline for the whole direction, a payload with many slots cannot
        // multiply the budget
        final long deadline = rule.detectorDeadlineNanos();
        boolean blocked = false;
        Map<String, Integer> auditCounts = new LinkedHashMap<>();
        boolean changed = false;
        for (PiiTextSlot slot : slots) {
            String text = slot.text();
            if (Objects.isNull(text) || text.isEmpty()) {
                continue;
            }
            List<PiiMatch> matches = detect(text, deadline);
            if (matches.isEmpty()) {
                continue;
            }
            if (anyBlock(matches)) {
                blocked = true;
                break;
            }
            changed |= redact(slot, text, matches, auditCounts);
        }
        List<PiiAuditRecord> audits = new ArrayList<>();
        for (Map.Entry<String, Integer> entry : auditCounts.entrySet()) {
            audits.add(new PiiAuditRecord(entry.getKey(), entry.getValue(), direction,
                    rule.actionFor(entry.getKey()).name(), rule.getRuleId()));
        }
        return new PiiOutcome(blocked, changed, audits);
    }

    private List<PiiMatch> detect(final String text, final long deadline) {
        List<PiiMatch> matches = new ArrayList<>();
        for (PiiDetector detector : rule.getDetectors()) {
            matches.addAll(detector.detect(text, deadline));
        }
        return matches;
    }

    private boolean anyBlock(final List<PiiMatch> matches) {
        for (PiiMatch match : matches) {
            if (rule.actionFor(match.getType()) == PiiAction.BLOCK) {
                return true;
            }
        }
        return false;
    }

    private boolean redact(final PiiTextSlot slot, final String text,
                           final List<PiiMatch> matches, final Map<String, Integer> auditCounts) {
        List<PiiMatch> ordered = new ArrayList<>(matches);
        ordered.sort(Comparator.comparingInt(PiiMatch::getStart).reversed()
                .thenComparing(match -> rule.actionFor(match.getType()).ordinal()));
        StringBuilder builder = new StringBuilder(text);
        int replacedStart = Integer.MAX_VALUE;
        boolean changed = false;
        for (PiiMatch match : ordered) {
            auditCounts.merge(match.getType(), 1, Integer::sum);
            if (rule.actionFor(match.getType()) != PiiAction.REDACT) {
                continue;
            }
            if (match.getEnd() > replacedStart) {
                continue;
            }
            builder.replace(match.getStart(), match.getEnd(), placeholder(match.getType()));
            replacedStart = match.getStart();
            changed = true;
        }
        if (changed) {
            slot.replace(builder.toString());
        }
        return changed;
    }

    private String placeholder(final String type) {
        return "<" + type + ">";
    }

    /**
     * The outcome of one direction scan.
     */
    static final class PiiOutcome {

        private final boolean blocked;

        private final boolean changed;

        private final List<PiiAuditRecord> audits;

        PiiOutcome(final boolean blocked, final boolean changed, final List<PiiAuditRecord> audits) {
            this.blocked = blocked;
            this.changed = changed;
            this.audits = audits;
        }

        /**
         * get whether the strictest action of the rule blocks the payload.
         *
         * @return true when the payload must be rejected
         */
        boolean isBlocked() {
            return blocked;
        }

        /**
         * get whether any slot was redacted, the caller re serializes the tree.
         *
         * @return true when at least one slot changed
         */
        boolean isChanged() {
            return changed;
        }

        /**
         * get the audit observations of the scan.
         *
         * @return the records, without detected values
         */
        List<PiiAuditRecord> getAudits() {
            return audits;
        }
    }
}
