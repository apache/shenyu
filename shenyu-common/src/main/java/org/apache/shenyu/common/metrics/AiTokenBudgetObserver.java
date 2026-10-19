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

package org.apache.shenyu.common.metrics;

/**
 * Nonblocking observation of reservation budgeting, without retaining request identities.
 */
@FunctionalInterface
public interface AiTokenBudgetObserver {

    /**
     * Record one budget event. Token amounts are meaningful for ADMITTED and USAGE_REPORTED.
     *
     * @param event bounded budget event
     * @param tokens estimated reserved or reliable reported completion tokens; otherwise zero
     */
    void record(Event event, long tokens);

    /**
     * Bounded labels; never include rule IDs, paths, principals or request IDs.
     */
    enum Event {
        /** A new reservation was acknowledged by Redis. */
        ADMITTED,
        /** Insufficient capacity prevented forwarding. */
        REJECTED,
        /** Reliable usage was observed, independently of Redis settlement success. */
        USAGE_REPORTED,
        /** Redis converted the reservation into actual usage. */
        SETTLED,
        /** A confirmed unforwarded reservation was released. */
        RELEASED,
        /** Unknown upstream usage retains the original reservation. */
        USAGE_HELD,
        /** Completion found that the original window no longer exists. */
        WINDOW_EXPIRED,
        /** A live window did not contain the expected reservation. */
        RESERVATION_MISSING,
        /** Window selection or admission failed. */
        ADMISSION_ERROR,
        /** Completion accounting failed or timed out. */
        ACCOUNTING_ERROR
    }
}
