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

package org.apache.shenyu.plugin.ai.pii.handler;

import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.plugin.ai.pii.PiiAction;
import org.apache.shenyu.plugin.ai.pii.detector.BuiltinPiiDetectors;
import org.apache.shenyu.plugin.ai.pii.detector.PiiDetector;
import org.apache.shenyu.plugin.ai.pii.detector.RegexPiiDetector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * A rule handle compiled into the immutable view the request path works with.
 * The compiled detectors, action overrides and policy switches are resolved
 * once per rule update, so a request only reads shared immutable state: two
 * concurrent requests never share redaction state.
 *
 * <p>Compilation is lenient by design: an invalid custom pattern or an unknown
 * action name is skipped with a warning instead of failing the whole rule, and
 * a custom pattern may not shadow a builtin detector type, because that would
 * silently drop the checksum validation of the builtin.</p>
 */
public final class PiiDetectionRule {

    private static final Logger LOG = LoggerFactory.getLogger(PiiDetectionRule.class);

    private final String ruleId;

    private final List<PiiDetector> detectors;

    private final Map<String, PiiAction> actions;

    private final PiiAction defaultAction;

    private final boolean scansRequest;

    private final boolean scansResponse;

    private final long maxBodySize;

    private final boolean failClosed;

    private final long detectorTimeoutMs;

    private PiiDetectionRule(final String ruleId, final List<PiiDetector> detectors,
                             final Map<String, PiiAction> actions, final PiiAction defaultAction,
                             final boolean scansRequest, final boolean scansResponse,
                             final long maxBodySize, final boolean failClosed, final long detectorTimeoutMs) {
        this.ruleId = ruleId;
        this.detectors = detectors;
        this.actions = actions;
        this.defaultAction = defaultAction;
        this.scansRequest = scansRequest;
        this.scansResponse = scansResponse;
        this.maxBodySize = maxBodySize;
        this.failClosed = failClosed;
        this.detectorTimeoutMs = detectorTimeoutMs;
    }

    /**
     * Compile a rule handle.
     *
     * @param ruleId the rule the handle belongs to, reported by audit records
     * @param handle the parsed rule handle
     * @return the compiled rule
     */
    public static PiiDetectionRule compile(final String ruleId, final PiiDetectionHandle handle) {
        PiiDetectionHandle effective = Objects.isNull(handle) ? PiiDetectionHandle.newDefaultInstance() : handle;
        List<PiiDetector> detectors = compileDetectors(effective);
        Map<String, PiiAction> actions = compileActions(effective);
        PiiAction compiledDefault = PiiAction.fromName(effective.getDefaultAction());
        if (Objects.isNull(compiledDefault)) {
            LOG.warn("pii detection rule {}: unknown defaultAction {}, falling back to {}", ruleId,
                    effective.getDefaultAction(), PiiDetectionHandle.DEFAULT_ACTION);
            compiledDefault = PiiAction.valueOf(PiiDetectionHandle.DEFAULT_ACTION);
        }
        return new PiiDetectionRule(ruleId, detectors, actions, compiledDefault,
                effective.scansRequest(), effective.scansResponse(),
                effective.getMaxBodySize(), effective.isFailClosed(), effective.getDetectorTimeoutMs());
    }

    private static List<PiiDetector> compileDetectors(final PiiDetectionHandle handle) {
        Map<String, PiiDetector> compiled = new LinkedHashMap<>(BuiltinPiiDetectors.select(handle.parsedDetectors()));
        for (Map.Entry<String, String> custom : handle.parsedCustomPatterns().entrySet()) {
            String type = custom.getKey();
            if (BuiltinPiiDetectors.all().containsKey(type)) {
                LOG.warn("pii detection: custom pattern type {} shadows a builtin detector, it is skipped", type);
                continue;
            }
            try {
                compiled.put(type, new RegexPiiDetector(type, Pattern.compile(custom.getValue())));
            } catch (PatternSyntaxException e) {
                LOG.warn("pii detection: custom pattern type {} has an invalid regex, it is skipped", type);
            }
        }
        return Collections.unmodifiableList(new ArrayList<>(compiled.values()));
    }

    private static Map<String, PiiAction> compileActions(final PiiDetectionHandle handle) {
        Map<String, PiiAction> compiled = new LinkedHashMap<>();
        for (Map.Entry<String, String> entry : handle.parsedActions().entrySet()) {
            PiiAction action = PiiAction.fromName(entry.getValue());
            if (Objects.isNull(action)) {
                LOG.warn("pii detection: unknown action {} for type {}, the override is skipped",
                        entry.getValue(), entry.getKey());
                continue;
            }
            compiled.put(entry.getKey(), action);
        }
        return Collections.unmodifiableMap(compiled);
    }

    /**
     * get the rule id, reported by audit records.
     *
     * @return the rule id
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * get the compiled detectors.
     *
     * @return the detectors, immutable
     */
    public List<PiiDetector> getDetectors() {
        return detectors;
    }

    /**
     * The action for a detected type: the override of the rule or its default
     * action.
     *
     * @param type the detected type
     * @return the action
     */
    public PiiAction actionFor(final String type) {
        PiiAction override = actions.get(type);
        return Objects.isNull(override) ? defaultAction : override;
    }

    /**
     * get whether the request direction is scanned.
     *
     * @return true when requests are scanned
     */
    public boolean scansRequest() {
        return scansRequest;
    }

    /**
     * get whether the response direction is scanned.
     *
     * @return true when responses are scanned
     */
    public boolean scansResponse() {
        return scansResponse;
    }

    /**
     * Whether a body size exceeds the bound of the rule. The bound is zero
     * when unlimited, a size that is not declared counts as unknown and never
     * matches, the caller checks it again once the body is read.
     *
     * @param size the size of the request or of its body
     * @return true when the size exceeds the bound
     */
    public boolean isOversized(final long size) {
        return maxBodySize > 0L && size > maxBodySize;
    }

    /**
     * get whether unsupported modes reject instead of passing through.
     *
     * @return true when unsupported modes reject
     */
    public boolean isFailClosed() {
        return failClosed;
    }

    /**
     * get the body size bound, zero means unlimited.
     *
     * @return the bound in bytes
     */
    public long getMaxBodySize() {
        return maxBodySize;
    }

    /**
     * The absolute deadline for one detector run over one text.
     *
     * @return the deadline in nanoseconds
     */
    public long detectorDeadlineNanos() {
        return System.nanoTime() + detectorTimeoutMs * 1_000_000L;
    }
}
