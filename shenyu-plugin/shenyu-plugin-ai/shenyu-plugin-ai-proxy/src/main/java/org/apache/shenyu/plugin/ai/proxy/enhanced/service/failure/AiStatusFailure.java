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

import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;

import java.util.Objects;

/**
 * Classified upstream HTTP failure with its single-owner response body preserved.
 *
 * @param response preserved upstream failure response
 * @param credentialFailure whether the response matched a declared credential failure
 */
public record AiStatusFailure(AiUpstreamResponse response, boolean credentialFailure) {

    private static final int MINIMUM_ERROR_STATUS = 400;

    public AiStatusFailure {
        Objects.requireNonNull(response, "response must not be null");
        if (response.getStatusCode() < MINIMUM_ERROR_STATUS) {
            throw new IllegalArgumentException("status failure requires an HTTP error response");
        }
    }
}
