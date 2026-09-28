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

package org.apache.shenyu.k8s.common;

import java.util.List;
import java.util.Objects;

/**
 * CRD bundle versions detected at startup with the SupportedVersion verdict: a missing
 * annotation or unrecognized version means unsupported; the controller still serves
 * best-effort because it only relies on the v1 GA surface.
 */
public final class GatewayApiCrdVersions {

    /** Oldest bundle version whose standard-channel CRDs serve ReferenceGrant at v1. */
    private static final int[] MIN_SUPPORTED = {1, 5};

    private final List<String> detectedVersions;

    private final boolean supported;

    public GatewayApiCrdVersions(final List<String> detectedVersions, final boolean supported) {
        this.detectedVersions = List.copyOf(detectedVersions);
        this.supported = supported;
    }

    /**
     * Whether every required CRD carries a recognized bundle version at or above the minimum.
     *
     * @return true when the install is fully supported
     */
    public boolean isSupported() {
        return supported;
    }

    /**
     * Distinct detected bundle-version annotation values, sorted.
     *
     * @return the detected versions, possibly empty when CRDs carry no annotation
     */
    public List<String> detectedVersions() {
        return detectedVersions;
    }

    /**
     * Parse {@code v1.5.1} into (major, minor); null when not numeric.
     *
     * @param version CRD bundle version string, e.g. {@code v1.5.1}
     * @return {major, minor} pair, or null when not numeric
     */
    public static int[] majorMinor(final String version) {
        if (Objects.isNull(version)) {
            return null;
        }
        String trimmed = version.startsWith("v") ? version.substring(1) : version;
        String[] parts = trimmed.split("\\.");
        if (parts.length < 2) {
            return null;
        }
        try {
            return new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])};
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    /**
     * Whether the parsed pair is at or above the minimum supported version.
     *
     * @param majorMinor parsed {major, minor} pair
     * @return true when at or above the minimum supported version
     */
    public static boolean atLeastMinSupported(final int[] majorMinor) {
        return Objects.nonNull(majorMinor)
                && (majorMinor[0] > MIN_SUPPORTED[0]
                || majorMinor[0] == MIN_SUPPORTED[0] && majorMinor[1] >= MIN_SUPPORTED[1]);
    }

    /**
     * Human-readable form for status messages, e.g. "v1.5.1, v1.6.0" or "none annotated".
     *
     * @return the detected versions joined for display
     */
    public String describeDetected() {
        return detectedVersions.isEmpty() ? "none annotated" : String.join(", ", detectedVersions);
    }
}
