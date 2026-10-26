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

import org.apache.shenyu.common.metrics.AgentMcpCallObserver.Outcome;
import reactor.util.context.Context;
import reactor.util.context.ContextView;

/**
 * Optional tracing bridge; each parsed tools/call owns one trace until its response terminates.
 */
@FunctionalInterface
public interface AgentMcpCallTracer {

    /**
     * Start one subscription's trace without receiving request data or identifiers.
     * @param parentContext subscription-local context
     * @return exclusively owned trace
     */
    Trace start(ContextView parentContext);

    /**
     * A request-local trace, independent of RPC ids and shared remote clients.
     */
    interface Trace {

        /**
         * Attach explicit execution context without installing a thread-local scope.
         * @param context downstream context
         * @return execution context
         */
        Context attach(Context context);

        /**
         * Finish after execution and response writing, including cancellation.
         * @param outcome bounded logical terminal outcome
         */
        void finish(Outcome outcome);
    }
}
