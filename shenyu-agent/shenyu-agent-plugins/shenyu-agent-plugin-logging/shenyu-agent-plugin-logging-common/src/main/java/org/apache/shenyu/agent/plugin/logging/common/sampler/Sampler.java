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

package org.apache.shenyu.agent.plugin.logging.common.sampler;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;

/**
 * sampler interface.
 */
public interface Sampler {

    Sampler ALWAYS_SAMPLE = request -> true;
    Sampler NEVER_SAMPLE = request -> false;

    /**
     * loops over the pre-canned decisions, resetting to zero when it gets to the end.
     *
     * @param request request
     * @return whether sample
     */
    boolean isSampled(ServerHttpRequest request);

    /**
     * create a sampler instance.
     *
     * @param probability probability
     * @return sampler instance
     */
    static Sampler create(String probability) {
        if (StringUtils.isBlank(probability)) {
            return ALWAYS_SAMPLE;
        }
        if ("0".equals(probability)) {
            return NEVER_SAMPLE;
        }
        if ("1".equals(probability) || "1.0".equals(probability) || "1.0.0".equals(probability)) {
            return ALWAYS_SAMPLE;
        }
        float parseProbability = NumberUtils.toFloat(probability, 1);
        if (parseProbability < 0.01f || parseProbability > 1) {
            throw new IllegalArgumentException(
                    "probability should be between 0.01 and 1: was " + probability);
        }
        return new CountingSampler(parseProbability);
    }
}
