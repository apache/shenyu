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

package org.apache.shenyu.admin.utils;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

/**
 * Security audit utilities for log-safe handling of sensitive values.
 */
public final class SecurityAuditUtils {

    /**
     * maximum length kept for a user-controlled value bound for a log line.
     */
    private static final int MAX_LOG_VALUE_LENGTH = 128;

    private SecurityAuditUtils() {
    }

    /**
     * build a one-way truncated SHA-256 fingerprint of a sensitive value for log correlation,
     * the raw value and the full digest are never logged.
     *
     * @param value sensitive value
     * @return truncated hex fingerprint, empty when unavailable
     */
    public static String fingerprint(final String value) {
        if (Objects.isNull(value)) {
            return "";
        }
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
            StringBuilder hex = new StringBuilder();
            for (int i = 0; i < 4 && i < digest.length; i++) {
                hex.append(String.format("%02x", digest[i]));
            }
            return hex.toString();
        } catch (NoSuchAlgorithmException e) {
            return "";
        }
    }

    /**
     * make a user-controlled value safe for a single log line: ISO control characters (including
     * CR and LF) are removed to prevent log forging, and the result is truncated to a bounded
     * length. The returned value is still untrusted user input, only log-line safe.
     *
     * @param value user-controlled value
     * @return sanitized value, empty when the input is null
     */
    public static String sanitize(final String value) {
        if (Objects.isNull(value)) {
            return "";
        }
        StringBuilder safe = new StringBuilder(Math.min(value.length(), MAX_LOG_VALUE_LENGTH));
        for (int i = 0; i < value.length() && safe.length() < MAX_LOG_VALUE_LENGTH; i++) {
            char c = value.charAt(i);
            if (!Character.isISOControl(c)) {
                safe.append(c);
            }
        }
        return safe.toString();
    }
}
