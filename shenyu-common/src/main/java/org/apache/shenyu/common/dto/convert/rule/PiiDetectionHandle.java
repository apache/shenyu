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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The rule handle of the pii detection plugin.
 *
 * <p>Every field is either a primitive or a delimited string, so the generic admin
 * rule form can edit the whole handle: {@code detectors} and {@code actions} are
 * comma separated ({@code "EMAIL,PHONE_CN"} / {@code "NATIONAL_ID_CN=BLOCK"}),
 * {@code customPatterns} holds one {@code TYPE=REGEX} entry per line and
 * {@code scope} is {@code "request"}, {@code "response"} or both. An empty
 * {@code detectors} value enables every builtin detector, and an empty
 * {@code actions} value applies {@code defaultAction} to every detection.</p>
 */
public class PiiDetectionHandle {

    /**
     * The default action applied to a detection whose type has no explicit action.
     */
    public static final String DEFAULT_ACTION = "REDACT";

    /**
     * The default scan scope, the request direction only.
     */
    public static final String DEFAULT_SCOPE = "request";

    /**
     * The default detector deadline in milliseconds, see {@link #detectorTimeoutMs}.
     */
    public static final long DEFAULT_DETECTOR_TIMEOUT_MS = 500L;

    private String detectors = "";

    private String customPatterns = "";

    private String actions = "";

    private String defaultAction = DEFAULT_ACTION;

    private String scope = DEFAULT_SCOPE;

    private long maxBodySize;

    private boolean failClosed;

    private long detectorTimeoutMs = DEFAULT_DETECTOR_TIMEOUT_MS;

    /**
     * A handle with every builtin detector enabled, redaction as the default action
     * and the request direction as the scope.
     *
     * @return the default handle
     */
    public static PiiDetectionHandle newDefaultInstance() {
        return new PiiDetectionHandle();
    }

    /**
     * The builtin detector types to enable, an empty list means all of them.
     *
     * @return the distinct detector types in configuration order
     */
    public List<String> parsedDetectors() {
        return splitToList(detectors);
    }

    /**
     * The admin defined detectors, one {@code TYPE=REGEX} entry per line.
     *
     * @return the entries as an ordered type to regex map, an invalid entry is skipped
     */
    public Map<String, String> parsedCustomPatterns() {
        if (Objects.isNull(customPatterns) || customPatterns.trim().isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, String> parsed = new LinkedHashMap<>();
        for (String line : customPatterns.split("\\n")) {
            int sep = line.indexOf('=');
            if (sep <= 0 || sep == line.length() - 1) {
                continue;
            }
            parsed.put(line.substring(0, sep).trim(), line.substring(sep + 1).trim());
        }
        return parsed;
    }

    /**
     * The per type action overrides, {@code "NATIONAL_ID_CN=BLOCK,EMAIL=REDACT"}.
     *
     * @return the type to action map, invalid entries are skipped
     */
    public Map<String, String> parsedActions() {
        Map<String, String> parsed = new LinkedHashMap<>();
        for (String entry : splitToList(actions)) {
            int sep = entry.indexOf('=');
            if (sep <= 0 || sep == entry.length() - 1) {
                continue;
            }
            parsed.put(entry.substring(0, sep).trim(), entry.substring(sep + 1).trim());
        }
        return parsed;
    }

    /**
     * Whether the request direction is scanned.
     *
     * @return true when the scope contains request
     */
    public boolean scansRequest() {
        return scopeContains("request");
    }

    /**
     * Whether the response direction is scanned.
     *
     * @return true when the scope contains response
     */
    public boolean scansResponse() {
        return scopeContains("response");
    }

    private boolean scopeContains(final String direction) {
        if (Objects.isNull(scope) || scope.trim().isEmpty()) {
            return DEFAULT_SCOPE.equals(direction);
        }
        for (String part : scope.split(",")) {
            if (direction.equalsIgnoreCase(part.trim())) {
                return true;
            }
        }
        return false;
    }

    private static List<String> splitToList(final String value) {
        if (Objects.isNull(value) || value.trim().isEmpty()) {
            return Collections.emptyList();
        }
        List<String> parsed = new ArrayList<>();
        for (String part : value.split(",")) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty() && !parsed.contains(trimmed)) {
                parsed.add(trimmed);
            }
        }
        return parsed;
    }

    /**
     * get the enabled builtin detector types.
     *
     * @return the detectors field
     */
    public String getDetectors() {
        return detectors;
    }

    /**
     * set the enabled builtin detector types.
     *
     * @param detectors the detectors field
     */
    public void setDetectors(final String detectors) {
        this.detectors = detectors;
    }

    /**
     * get the custom detector patterns.
     *
     * @return the customPatterns field
     */
    public String getCustomPatterns() {
        return customPatterns;
    }

    /**
     * set the custom detector patterns.
     *
     * @param customPatterns the customPatterns field
     */
    public void setCustomPatterns(final String customPatterns) {
        this.customPatterns = customPatterns;
    }

    /**
     * get the per type action overrides.
     *
     * @return the actions field
     */
    public String getActions() {
        return actions;
    }

    /**
     * set the per type action overrides.
     *
     * @param actions the actions field
     */
    public void setActions(final String actions) {
        this.actions = actions;
    }

    /**
     * get the action applied when a type has no override.
     *
     * @return the defaultAction field
     */
    public String getDefaultAction() {
        return defaultAction;
    }

    /**
     * set the action applied when a type has no override.
     *
     * @param defaultAction the defaultAction field
     */
    public void setDefaultAction(final String defaultAction) {
        this.defaultAction = defaultAction;
    }

    /**
     * get the scanned directions.
     *
     * @return the scope field
     */
    public String getScope() {
        return scope;
    }

    /**
     * set the scanned directions.
     *
     * @param scope the scope field
     */
    public void setScope(final String scope) {
        this.scope = scope;
    }

    /**
     * get the body size bound, zero means unlimited.
     *
     * @return the maxBodySize field
     */
    public long getMaxBodySize() {
        return maxBodySize;
    }

    /**
     * set the body size bound, zero means unlimited.
     *
     * @param maxBodySize the maxBodySize field
     */
    public void setMaxBodySize(final long maxBodySize) {
        this.maxBodySize = maxBodySize;
    }

    /**
     * get whether unsupported modes reject instead of passing through.
     *
     * @return the failClosed field
     */
    public boolean isFailClosed() {
        return failClosed;
    }

    /**
     * set whether unsupported modes reject instead of passing through.
     *
     * @param failClosed the failClosed field
     */
    public void setFailClosed(final boolean failClosed) {
        this.failClosed = failClosed;
    }

    /**
     * get the detector deadline in milliseconds.
     *
     * @return the detectorTimeoutMs field
     */
    public long getDetectorTimeoutMs() {
        return detectorTimeoutMs;
    }

    /**
     * set the detector deadline in milliseconds.
     *
     * @param detectorTimeoutMs the detectorTimeoutMs field
     */
    public void setDetectorTimeoutMs(final long detectorTimeoutMs) {
        this.detectorTimeoutMs = detectorTimeoutMs;
    }
}
