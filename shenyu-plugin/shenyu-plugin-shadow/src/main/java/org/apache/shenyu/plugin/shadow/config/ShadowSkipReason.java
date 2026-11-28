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

package org.apache.shenyu.plugin.shadow.config;

/**
 * Why one eligible request was not mirrored.
 *
 * <p>Skipped work is counted instead of silently disappearing, so an operator
 * can separate an unsampled route from a bound that actually rejected traffic.</p>
 */
public enum ShadowSkipReason {

    /**
     * The request is not an HTTP request.
     */
    NOT_HTTP,

    /**
     * The request method is outside the rule's method allowlist.
     */
    METHOD_NOT_ALLOWED,

    /**
     * The request is a streaming or upgraded connection.
     */
    STREAMING_REQUEST,

    /**
     * The request body exceeded the configured byte bound.
     */
    BODY_TOO_LARGE,

    /**
     * The primary did not read the complete body declared by the request.
     */
    BODY_INCOMPLETE,

    /**
     * The primary exchange was cancelled before it terminated normally.
     */
    PRIMARY_CANCELLED,

    /**
     * The shadow target cannot be combined with the request path and query.
     */
    INVALID_TARGET,

    /**
     * The rule's bounded shadow queue was full.
     */
    QUEUE_OVERFLOW
}
