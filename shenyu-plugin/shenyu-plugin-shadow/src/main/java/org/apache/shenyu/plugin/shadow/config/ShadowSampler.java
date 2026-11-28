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

package org.apache.shenyu.plugin.shadow.config;

import org.apache.shenyu.common.dto.RuleData;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/**
 * Request-identifier-based sampling.
 *
 * <p>The decision is derived from the request identifier and the rule id, so a
 * given request is always judged the same way within one evaluation and
 * different requests are sampled independently. It is not stable across a
 * re-created request: a retry or a new connection carries a new identifier and
 * may fall on the other side of the sample rate.</p>
 */
public final class ShadowSampler {

    private static final int PRECISION = 10_000;

    private ShadowSampler() {
    }

    /**
     * Decide whether this request is sampled for the rule.
     *
     * @param exchange the primary exchange
     * @param rule the matched rule
     * @param rate the sampling rate in [0, 1]
     * @return true when the request is mirrored
     */
    public static boolean sample(final ServerWebExchange exchange, final RuleData rule, final Double rate) {
        if (Objects.isNull(rate) || rate <= 0D) {
            return false;
        }
        if (rate >= 1D) {
            return true;
        }
        String identity = exchange.getRequest().getId() + rule.getId();
        int threshold = (int) Math.round(rate * PRECISION);
        return Math.floorMod(identity.hashCode(), PRECISION) < threshold;
    }
}
