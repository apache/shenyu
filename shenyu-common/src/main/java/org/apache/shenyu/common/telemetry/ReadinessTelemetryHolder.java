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

package org.apache.shenyu.common.telemetry;

/**
 * Global holder for the active instance readiness telemetry provider.
 */
public final class ReadinessTelemetryHolder {

    private static volatile ReadinessTelemetryProvider provider;

    private ReadinessTelemetryHolder() {
    }

    /**
     * Set the active readiness telemetry provider.
     *
     * @param activeProvider provider
     */
    public static void setProvider(final ReadinessTelemetryProvider activeProvider) {
        provider = activeProvider;
    }

    /**
     * Get the active readiness telemetry provider.
     *
     * @return provider or null if uninitialized
     */
    public static ReadinessTelemetryProvider getProvider() {
        return provider;
    }

    /**
     * Reset the active provider.
     */
    public static void reset() {
        provider = null;
    }
}
