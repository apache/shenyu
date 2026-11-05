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

import org.apache.shenyu.common.enums.InstanceStatusEnum;

/**
 * Lightweight, non-blocking provider of instance readiness telemetry.
 */
@FunctionalInterface
public interface ReadinessTelemetryProvider {

    /**
     * Maximum length for diagnostic reason codes to strictly bound payload size.
     */
    int MAX_REASON_CODE_LENGTH = 64;

    /**
     * Get real-time instance readiness status name (e.g. {@link InstanceStatusEnum#READY}).
     *
     * @return readiness status name
     */
    String getReadinessStatus();

    /**
     * Get optional failure reason code.
     *
     * @return reason code, or null if ready or initializing normally
     */
    default String getReasonCode() {
        return null;
    }
}
