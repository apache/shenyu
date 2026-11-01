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

package org.apache.shenyu.plugin.base.utils;

/**
 * A read-only, nonblocking observer of upstream transport attempts in the standard
 * HTTP forwarding path (#8209). Implementations must never throw, block, mutate the
 * request or the selected target, restart work or subscribe to response bodies — a
 * throwing or slow observer is isolated by the transport path, but implementations
 * are expected to hand facts to their own bounded facilities.
 *
 * <p>Registration is opt-in through {@link UpstreamAttemptObserverHolder}; with no
 * observer registered the transport path allocates nothing.
 */
public interface UpstreamAttemptObserver {

    /**
     * An admitted transport attempt started. One subscription of the transport
     * publisher is one attempt.
     *
     * @param observation the live observation of this attempt
     */
    void onAttemptStarted(UpstreamAttemptObservation observation);

    /**
     * The upstream response headers became ready. This is not response completion:
     * the body may still be streaming, and a later error means the attempt did not
     * fully succeed.
     *
     * @param observation the live observation of this attempt
     */
    void onHeadersReady(UpstreamAttemptObservation observation);

    /**
     * The attempt reached its terminal outcome — exactly once per attempt. For the
     * serving attempt this happens when the response body is done (completed, failed
     * or cancelled downstream); a failed attempt finishes as soon as it terminates.
     *
     * @param observation the finished observation
     */
    void onAttemptFinished(UpstreamAttemptObservation observation);
}
