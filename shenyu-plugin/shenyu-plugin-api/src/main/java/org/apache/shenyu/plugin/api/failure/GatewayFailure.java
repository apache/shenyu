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

package org.apache.shenyu.plugin.api.failure;

import java.util.Objects;

/**
 * Body-free classification, separate from an HTTP presentation and internal causes.
 * @param reason closed logical reason
 * @param stage actual adopted owner
 * @param protocol HTTP-facing context
 * @param output response state observed by that owner
 * @param plugin bounded local plugin identity, never request input
 */
public record GatewayFailure(Reason reason, Stage stage, Protocol protocol, Output output, String plugin) {

    public GatewayFailure {
        Objects.requireNonNull(reason);
        Objects.requireNonNull(stage);
        Objects.requireNonNull(protocol);
        Objects.requireNonNull(output);
        plugin = Objects.isNull(plugin) ? "" : plugin.substring(0, Math.min(128, plugin.length())).replaceAll("[\\p{Cntrl}]", "_");
    }

    /** Closed failure vocabulary; never derived from Throwable messages. */
    public enum Reason {
        SELECTOR_MISS, RULE_MISS, INVALID_REQUEST, AUTHENTICATION, AUTHORIZATION, RESOURCE_LIMIT, DEADLINE, CANCELLATION, UPSTREAM, UNKNOWN
    }

    /** Finite first-delivery adoption list. */
    public enum Stage {
        GLOBAL_HANDLER, SELECTOR_MATCH, RULE_MATCH
    }

    /** Native protocol terminal rendering remains outside this HTTP-facing contract. */
    public enum Protocol {
        HTTP, HTTP_BRIDGE, UNKNOWN
    }

    /** A pre-commit observation alone never grants a concurrent writer permission. */
    public enum Output {
        BEFORE_ERROR_WRITE, STARTED_OR_COMMITTED
    }
}
