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
 * Explicit upstream token counts, never estimates. Null means unknown, not zero.
 * Total is never calculated from the other fields. A snapshot does not establish
 * chargeability or successful delivery, especially on a cancelled stream.
 * @param promptTokens upstream-reported input count
 * @param completionTokens upstream-reported output count
 * @param totalTokens upstream-reported total
 * @param status bounded observation quality
 */
public record AiUsage(Long promptTokens, Long completionTokens, Long totalTokens, Status status) {

    public static final AiUsage UNKNOWN = new AiUsage(null, null, null, Status.UNKNOWN);

    /**
     * Quality of the raw upstream observation, not a billing decision.
     */
    public enum Status {
        UNKNOWN, REPORTED, INVALID, LIMIT_EXCEEDED, UNSUPPORTED
    }
}
