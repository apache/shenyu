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

package org.apache.shenyu.plugin.shadow.dispatch;

import org.springframework.http.HttpHeaders;

import java.net.URI;

/**
 * One immutable shadow call.
 *
 * <p>The task owns a copy of everything the shadow target needs, so dispatch is
 * independent of the primary exchange lifetime.</p>
 */
public final class ShadowTask {

    private final String method;

    private final URI uri;

    private final HttpHeaders headers;

    private final byte[] body;

    private final int timeoutMillis;

    public ShadowTask(final String method, final URI uri, final HttpHeaders headers,
                      final byte[] body, final int timeoutMillis) {
        this.method = method;
        this.uri = uri;
        this.headers = headers;
        this.body = body;
        this.timeoutMillis = timeoutMillis;
    }

    /**
     * Gets the HTTP method.
     *
     * @return the HTTP method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Gets the shadow destination URI.
     *
     * @return the shadow destination URI
     */
    public URI getUri() {
        return uri;
    }

    /**
     * Gets the forwarded headers.
     *
     * @return the forwarded headers
     */
    public HttpHeaders getHeaders() {
        return headers;
    }

    /**
     * Gets the request body copy.
     *
     * @return the request body copy
     */
    public byte[] getBody() {
        return body;
    }

    /**
     * Gets the attempt budget.
     *
     * @return the attempt budget in milliseconds
     */
    public int getTimeoutMillis() {
        return timeoutMillis;
    }
}
