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

package org.apache.shenyu.web.contract;

/**
 * The role a plugin plays in the legacy plugin chain.
 *
 * <p>A profile describes the expected continuation shape, not a new plugin interface. Select the
 * closest profile explicitly; a wrapper that decorates or resubscribes downstream is not a
 * pass-through plugin.</p>
 */
public enum PluginContractProfile {

    /**
     * Delegates once to the downstream chain.
     */
    PASS_THROUGH("pass-through", true),

    /**
     * Mutates the exchange and delegates once to the downstream chain.
     */
    TRANSFORM("transform", true),

    /**
     * Owns the response and completes without delegating downstream.
     */
    SHORT_CIRCUIT("short-circuit", false),

    /**
     * Decorates or resubscribes the downstream publisher.
     */
    DOWNSTREAM_WRAPPER("downstream-wrapper", true),

    /**
     * Uses {@code AbstractShenyuPlugin} selector and rule matching.
     */
    SELECTOR_RULE("selector-rule", true);

    private final String displayName;

    private final boolean delegatesDownstream;

    PluginContractProfile(final String displayName, final boolean delegatesDownstream) {
        this.displayName = displayName;
        this.delegatesDownstream = delegatesDownstream;
    }

    /**
     * Returns the stable profile name used in diagnostics.
     *
     * @return profile name
     */
    public String displayName() {
        return displayName;
    }

    /**
     * Returns whether a matching plugin is expected to reach the downstream chain at least once.
     *
     * @return true when downstream delegation is expected
     */
    public boolean delegatesDownstream() {
        return delegatesDownstream;
    }
}
