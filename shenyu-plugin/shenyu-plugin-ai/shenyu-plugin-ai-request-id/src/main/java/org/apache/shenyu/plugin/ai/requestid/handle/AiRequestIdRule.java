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

package org.apache.shenyu.plugin.ai.requestid.handle;

import java.util.Objects;

/**
 * Immutable, validated AI request id rule handle.
 */
public final class AiRequestIdRule {

    private final String rawHandle;

    private final boolean responseHeaderEnabled;

    private final boolean valid;

    private final String errorMessage;

    private AiRequestIdRule(final String rawHandle, final boolean responseHeaderEnabled,
                            final boolean valid, final String errorMessage) {
        this.rawHandle = rawHandle;
        this.responseHeaderEnabled = responseHeaderEnabled;
        this.valid = valid;
        this.errorMessage = errorMessage;
    }

    /**
     * Create a valid rule.
     *
     * @param rawHandle raw rule handle
     * @param responseHeaderEnabled whether the canonical response header is returned
     * @return valid rule
     */
    public static AiRequestIdRule valid(final String rawHandle, final boolean responseHeaderEnabled) {
        return new AiRequestIdRule(rawHandle, responseHeaderEnabled, true, null);
    }

    /**
     * Create the default rule used by a non-continued selector that has no rule handle.
     *
     * @return default valid rule
     */
    public static AiRequestIdRule defaultRule() {
        return valid(null, true);
    }

    /**
     * Create an invalid rule carrying a stable, non-sensitive reason.
     *
     * @param rawHandle raw rule handle
     * @param errorMessage validation reason
     * @return invalid rule
     */
    public static AiRequestIdRule invalid(final String rawHandle, final String errorMessage) {
        return new AiRequestIdRule(rawHandle, false, false, errorMessage);
    }

    /**
     * Get rawHandle.
     *
     * @return rawHandle
     */
    public String getRawHandle() {
        return rawHandle;
    }

    /**
     * Get responseHeaderEnabled.
     *
     * @return responseHeaderEnabled
     */
    public boolean isResponseHeaderEnabled() {
        return responseHeaderEnabled;
    }

    /**
     * Get valid.
     *
     * @return valid
     */
    public boolean isValid() {
        return valid;
    }

    /**
     * Get errorMessage.
     *
     * @return errorMessage
     */
    public String getErrorMessage() {
        return errorMessage;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiRequestIdRule that = (AiRequestIdRule) o;
        return responseHeaderEnabled == that.responseHeaderEnabled && valid == that.valid
                && Objects.equals(rawHandle, that.rawHandle) && Objects.equals(errorMessage, that.errorMessage);
    }

    @Override
    public int hashCode() {
        return Objects.hash(rawHandle, responseHeaderEnabled, valid, errorMessage);
    }
}
