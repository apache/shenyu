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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service.failure;

/**
 * Describes an upstream HTTP status that may be propagated after retry or fallback orchestration.
 */
public final class AiUpstreamStatusException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final boolean credentialFailure;

    /**
     * Create an upstream status exception.
     *
     * @param status upstream HTTP status
     */
    public AiUpstreamStatusException(final int status) {
        this(status, false);
    }

    /**
     * Create an upstream status exception.
     *
     * @param status upstream HTTP status
     * @param credentialFailure whether the response matched a credential failure declaration
     */
    public AiUpstreamStatusException(final int status, final boolean credentialFailure) {
        super("AI upstream returned HTTP " + status);
        this.credentialFailure = credentialFailure;
    }

    public boolean credentialFailure() {
        return credentialFailure;
    }

}
