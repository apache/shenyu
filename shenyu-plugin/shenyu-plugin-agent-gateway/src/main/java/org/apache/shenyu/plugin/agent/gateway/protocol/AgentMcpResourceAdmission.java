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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import org.apache.shenyu.common.dto.AgentGatewayAdmissionConfig;
import org.apache.shenyu.common.metrics.AgentAdmissionMetrics;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.Objects;

/** Optional resource-admission port; the no-op implementation preserves existing behavior. */
public interface AgentMcpResourceAdmission {

    AgentMcpResourceAdmission NOOP = new AgentMcpResourceAdmission() {
        @Override
        public <T> Mono<T> withLocal(final Instant deadline, final Mono<T> action) {
            return action;
        }

        @Override
        public <T> Mono<T> withRemote(final String server, final Instant deadline, final Mono<T> action) {
            return action;
        }
    };

    /** Bound one local-provider execution.
     * @param deadline absolute request deadline
     * @param action controlled execution
*    @param <T> result type
     * @return controlled result
     */
    <T> Mono<T> withLocal(Instant deadline, Mono<T> action);

    /** Bound one configured remote-server execution.
     * @param server stable target name, never Agent input
     * @param deadline absolute request deadline
     * @param action controlled execution
*    @param <T> result type
     * @return controlled result
     */
    <T> Mono<T> withRemote(String server, Instant deadline, Mono<T> action);

    /** Wrap the concrete deterministic controller while retaining the small port.
     * @param config trusted hard-bounded configuration
     * @return closeable port implementation
     */
    static ResourceAdmissionController create(final AgentGatewayAdmissionConfig config) {
        return new ResourceAdmissionController(new AgentResourceAdmission(config));
    }

    /** Closeable wrapper used by the Spring lifecycle. */
    final class ResourceAdmissionController implements AgentMcpResourceAdmission, AutoCloseable {

        private final AgentResourceAdmission delegate;

        private volatile AgentResourceAdmission current;

        private ResourceAdmissionController(final AgentResourceAdmission delegate) {
            this.delegate = delegate;
            this.current = delegate;
        }

        @Override
        public <T> Mono<T> withLocal(final Instant deadline, final Mono<T> action) {
            return current.withLocal(deadline, action);
        }

        @Override
        public <T> Mono<T> withRemote(final String server, final Instant deadline, final Mono<T> action) {
            return current.withRemote(server, deadline, action);
        }

        /**
         * Apply replacement limits without replacing the shared accounting state.
         * Requests owned by the active controller remain admitted or queued across reloads.
         *
         * @param replacement validated replacement controller
         */
        public void replace(final ResourceAdmissionController replacement) {
            Objects.requireNonNull(replacement, "replacement");
            if (replacement.delegate.closed()) {
                replacement.delegate.close();
                return;
            }
            try {
                current.updateLimits(replacement.delegate);
            } finally {
                replacement.delegate.close();
            }
        }

        @Override
        public void close() {
            AgentResourceAdmission active;
            synchronized (this) {
                active = current;
            }
            active.close();
        }

        /**
         * Read the currently active limits without creating another controller.
         * @return immutable trusted limits
         */
        public AgentGatewayAdmissionConfig config() {
            return current.config();
        }

        /**
         * Read the live states of the active controller.
         * @return immutable controller snapshots
         */
        public java.util.List<AgentAdmissionMetrics.Snapshot> snapshot() {
            return current.snapshot();
        }
    }
}
