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

package org.apache.shenyu.plugin.sign.api;

import com.google.common.base.Objects;

public final class VerifyResult {

    private static final VerifyResult SUCCESS = new VerifyResult(true, "success");

    private final boolean success;

    private final String reason;

    private VerifyResult(final boolean success, final String reason) {
        this.success = success;
        this.reason = reason;
    }

    /**
     * Get the reason for the failure.
     *
     * @return reason
     */
    public String getReason() {
        return reason;
    }

    /**
     * success or not.
     *
     * @return true or false
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * Failure or not.
     *
     * @return true or false
     */
    public boolean isFailed() {
        return !success;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        VerifyResult result = (VerifyResult) o;
        return success == result.success && Objects.equal(reason, result.reason);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(success, reason);
    }

    /**
     * Generate failed verifyResult.
     *
     * @param reason The reason for the failure
     * @return verifyResult
     */
    public static VerifyResult fail(final String reason) {
        return new VerifyResult(false, reason);
    }

    /**
     * Generate successful verifyResult.
     *
     * @return verifyResult
     */
    public static VerifyResult success() {
        return SUCCESS;
    }
}
