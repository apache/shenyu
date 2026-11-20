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

import java.util.List;

/**
 * Optional application-owned, nonblocking sink. Implementations must bound queues
 * and must not use request identities as unrestricted metric labels.
 */
@FunctionalInterface
public interface AiCallObserver {

    /**
     * Select one explicit composition without silently fanning out arbitrary duplicate sinks.
     * @param candidates application observers
     * @return selected observer, or null when observation is disabled
     */
    static AiCallObserver select(final List<AiCallObserver> candidates) {
        List<AiCallObserver> composites = candidates.stream().filter(Composite.class::isInstance).toList();
        if (composites.size() == 1) {
            return composites.get(0);
        }
        if (composites.size() > 1 || candidates.size() > 1) {
            throw new IllegalStateException("AI observation requires one sink or one explicit composite");
        }
        return candidates.isEmpty() ? null : candidates.get(0);
    }

    /**
     * Receive the single logical request terminal snapshot.
     * @param event immutable snapshot without payloads or credentials
     */
    void record(AiTerminalEvent event);

    /**
     * Receive one snapshot for each actually started upstream attempt.
     * @param event immutable attempt snapshot, not another logical request
     */
    default void recordAttempt(final AiAttemptEvent event) {
    }

    /** Application-declared canonical composition; implementations own delegate deduplication. */
    interface Composite extends AiCallObserver {
    }
}
