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

package org.apache.shenyu.plugin.httpclient.exception;

/**
 * Signals an upstream status that should trigger failover.
 */
public final class ShenyuUpstreamStatusException extends RuntimeException {

    private static final long serialVersionUID = 660353739127205812L;

    /**
     * Instantiates a new upstream status exception.
     *
     * @param statusCode upstream HTTP status code
     */
    public ShenyuUpstreamStatusException(final int statusCode) {
        super("Upstream returned retryable HTTP status " + statusCode);
    }
}
