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

import java.util.Objects;

/**
 * This is the AI request id rule handle.
 */
public class AiRequestIdHandle {

    /**
     * Whether the canonical AI request id response header is returned.
     */
    private Boolean responseHeaderEnabled;

    /**
     * Create a handle with the documented defaults.
     *
     * @return default handle
     */
    public static AiRequestIdHandle newDefaultInstance() {
        AiRequestIdHandle handle = new AiRequestIdHandle();
        handle.setResponseHeaderEnabled(Boolean.TRUE);
        return handle;
    }

    /**
     * Get responseHeaderEnabled.
     *
     * @return responseHeaderEnabled
     */
    public Boolean getResponseHeaderEnabled() {
        return responseHeaderEnabled;
    }

    /**
     * Set responseHeaderEnabled.
     *
     * @param responseHeaderEnabled responseHeaderEnabled
     */
    public void setResponseHeaderEnabled(final Boolean responseHeaderEnabled) {
        this.responseHeaderEnabled = responseHeaderEnabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiRequestIdHandle that = (AiRequestIdHandle) o;
        return Objects.equals(responseHeaderEnabled, that.responseHeaderEnabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(responseHeaderEnabled);
    }

    @Override
    public String toString() {
        return "AiRequestIdHandle{" + "responseHeaderEnabled=" + responseHeaderEnabled + '}';
    }
}
