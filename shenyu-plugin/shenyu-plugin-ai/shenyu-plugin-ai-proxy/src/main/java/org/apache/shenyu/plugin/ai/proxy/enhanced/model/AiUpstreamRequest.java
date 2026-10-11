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

package org.apache.shenyu.plugin.ai.proxy.enhanced.model;

import java.net.URI;
import java.util.List;
import java.util.Map;

/**
 * Protocol-independent request sent to an AI upstream.
 */
public final class AiUpstreamRequest {

    private URI uri;

    private String method;

    private Map<String, List<String>> headers;

    private byte[] body;

    /**
     * Get request URI.
     *
     * @return request URI
     */
    public URI getUri() {
        return uri;
    }

    /**
     * Set request URI.
     *
     * @param uri request URI
     */
    public void setUri(final URI uri) {
        this.uri = uri;
    }

    /**
     * Get HTTP method.
     *
     * @return HTTP method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Set HTTP method.
     *
     * @param method HTTP method
     */
    public void setMethod(final String method) {
        this.method = method;
    }

    /**
     * Get request headers.
     *
     * @return request headers
     */
    public Map<String, List<String>> getHeaders() {
        return headers;
    }

    /**
     * Set request headers.
     *
     * @param headers request headers
     */
    public void setHeaders(final Map<String, List<String>> headers) {
        this.headers = headers;
    }

    /**
     * Get request body.
     *
     * @return request body
     */
    public byte[] getBody() {
        return body;
    }

    /**
     * Set request body.
     *
     * @param body request body
     */
    public void setBody(final byte[] body) {
        this.body = body;
    }
}
