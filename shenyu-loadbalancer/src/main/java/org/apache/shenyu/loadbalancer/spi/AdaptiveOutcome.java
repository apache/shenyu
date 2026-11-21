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

package org.apache.shenyu.loadbalancer.spi;

/**
 * Classified outcome of one transport attempt, fed back into the adaptive scoring.
 *
 * <p>The classes keep upstream failures distinct from client-caused results: a fast
 * 5xx penalizes the serving node, while a 4xx (application or authentication
 * rejection) is neutral — it must never make a node look better or worse, and a
 * downstream cancellation says nothing about the upstream at all.</p>
 */
public enum AdaptiveOutcome {

    /**
     * The attempt completed with a successful response class.
     */
    SUCCESS,

    /**
     * The upstream answered with a client-error class (4xx): neutral, no latency is
     * recorded and no penalty is applied.
     */
    CLIENT_ERROR,

    /**
     * The upstream answered with a server-error class (5xx): the latency is real but
     * the fast failure must not look attractive, so a penalty is applied.
     */
    SERVER_ERROR,

    /**
     * The attempt failed on the transport (connection, timeout, cancellation of the
     * write): no latency is observable, a penalty is applied.
     */
    TRANSPORT_ERROR
}
