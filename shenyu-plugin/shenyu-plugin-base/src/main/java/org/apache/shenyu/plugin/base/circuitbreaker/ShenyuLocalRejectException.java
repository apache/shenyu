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

package org.apache.shenyu.plugin.base.circuitbreaker;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

/**
 * A rejection produced by the gateway itself before the request reached the upstream,
 * e.g. the divide request-body limit. The circuit breaker must not count these as
 * upstream failures, so they are classified by this marker type rather than by the
 * HTTP status — upstream-semantics statuses such as 408 (mapped by the retry strategy
 * from exhausted upstream timeouts) must keep counting.
 */
public class ShenyuLocalRejectException extends ResponseStatusException {

    public ShenyuLocalRejectException(final HttpStatus status, final String reason) {
        super(status, reason);
    }

    public ShenyuLocalRejectException(final HttpStatus status, final String reason, final Throwable cause) {
        super(status, reason, cause);
    }
}
