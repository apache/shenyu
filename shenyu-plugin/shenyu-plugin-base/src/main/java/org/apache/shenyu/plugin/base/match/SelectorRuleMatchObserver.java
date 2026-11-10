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

package org.apache.shenyu.plugin.base.match;

/**
 * Read-only consumer of selector/rule matching decisions.
 *
 * <p>Implementations are invoked synchronously on the gateway request path after a matching
 * decision has been taken, on the event-loop thread that is driving the request. The
 * contract is strict: the callback must return immediately (blocking there directly slows
 * the real request), must not throw, must not mutate the exchange, the configuration caches
 * or the observation, and must never consume or buffer request bodies. Sampling, bounded
 * queues and export backpressure are the consumer's own responsibility, so a slow or
 * saturated consumer can never alter a request outcome, hold an unbounded queue or retain
 * request data.</p>
 */
@FunctionalInterface
public interface SelectorRuleMatchObserver {

    /**
     * Receive one immutable matching-decision summary.
     *
     * @param observation the bounded, read-only decision summary
     */
    void onMatchDecision(SelectorRuleMatchObservation observation);
}
