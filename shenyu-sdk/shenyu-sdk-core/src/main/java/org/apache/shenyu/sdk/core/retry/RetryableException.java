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

package org.apache.shenyu.sdk.core.retry;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuRequest.HttpMethod;
import org.apache.shenyu.sdk.core.ShenyuResponse;

import java.util.Date;
import java.util.Optional;

/**
 * This exception is raised when the {@link org.apache.shenyu.sdk.core.ShenyuResponse} is deemed to be retryable, typically via an
 * {@link org.apache.shenyu.common.exception.CommonErrorCode} when the {@link ShenyuResponse#getStatus() status} is 503.
 * Reference to feign.RetryableException.
 */
public class RetryableException extends ShenyuException {

    private static final long serialVersionUID = 9156957575330268250L;

    private final Long retryAfter;

    private final HttpMethod httpMethod;

    /**
     * construct function.
     *
     * @param message message
     * @param cause cause
     * @param retryAfter usually corresponds to the retryAfter header.
     * @param request request
     */
    public RetryableException(final String message, final Throwable cause,
                              final Date retryAfter, final ShenyuRequest request) {
        super(message, cause);
        this.httpMethod = request.getHttpMethod();
        this.retryAfter = Optional.ofNullable(retryAfter).map(Date::getTime).orElse(null);
    }

    /**
     * Sometimes corresponds to the retryAfter header present in {@code 503}
     * status. Other times parsed from an application-specific response. Null if unknown.
     *
     * @return {@linkplain Date}
     */
    public Date retryAfter() {
        return Optional.ofNullable(retryAfter).map(Date::new).orElse(null);
    }

    /**
     * get httpMethod.
     *
     * @return {@linkplain HttpMethod}
     */
    public HttpMethod method() {
        return this.httpMethod;
    }
}
