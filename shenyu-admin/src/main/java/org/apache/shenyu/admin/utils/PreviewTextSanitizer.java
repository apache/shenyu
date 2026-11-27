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

import java.util.Objects;

/**
 * Bounds and sanitizes text included in preview reports and logs.
 */
public final class PreviewTextSanitizer {

    private PreviewTextSanitizer() {
    }

    /**
     * Remove control characters and cap the value length.
     *
     * @param value raw value
     * @param maxLength maximum output length
     * @return sanitized value
     */
    public static String sanitize(final String value, final int maxLength) {
        if (Objects.isNull(value) || maxLength <= 0) {
            return "";
        }
        int limit = Math.min(value.length(), maxLength);
        if (limit > 0 && limit < value.length() && Character.isHighSurrogate(value.charAt(limit - 1))) {
            limit--;
        }
        StringBuilder result = new StringBuilder(limit);
        for (int index = 0; index < limit; index++) {
            char current = value.charAt(index);
            result.append(Character.isISOControl(current) ? ' ' : current);
        }
        return result.toString();
    }
}
