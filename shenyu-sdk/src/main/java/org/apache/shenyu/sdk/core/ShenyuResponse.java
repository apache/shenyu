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

package org.apache.shenyu.sdk.core;

import java.util.Collection;
import java.util.Map;


/**
 * ShenyuResponse.
 */
public final class ShenyuResponse {

    private final int status;

    private final String reason;

    private final Map<String, Collection<String>> headers;

    private final String body;

    private final ShenyuRequest request;

    /**
     * ShenyuResponse.
     *
     * @param status status
     * @param reason reason
     * @param headers headers
     * @param body body
     * @param request request
     */
    public ShenyuResponse(final int status, final String reason, final Map<String, Collection<String>> headers,
                          final String body, final ShenyuRequest request) {
        this.status = status;
        this.reason = reason;
        this.headers = headers;
        this.body = body;
        this.request = request;
    }

    /**
     * getStatus.
     *
     * @return {@link int}
     */
    public int getStatus() {
        return status;
    }

    /**
     * getReason.
     *
     * @return {@link String}
     */
    public String getReason() {
        return reason;
    }

    /**
     * getHeaders.
     *
     * @return {@link Map}
     */
    public Map<String, Collection<String>> getHeaders() {
        return headers;
    }

    /**
     * getBody.
     *
     * @return {@link String}
     */
    public String getBody() {
        return body;
    }

    /**
     * getRequest.
     *
     * @return {@link ShenyuRequest}
     */
    public ShenyuRequest getRequest() {
        return request;
    }
}
