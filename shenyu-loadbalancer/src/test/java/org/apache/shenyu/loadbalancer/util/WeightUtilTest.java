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

package org.apache.shenyu.loadbalancer.util;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * calculate weight util test.
 */
public class WeightUtilTest {

    @Test
    public void testCalculateTotalWeight() {
        List<Upstream> upstreams = Stream.of(1, 2, 3)
                .map(weight -> Upstream.builder().url("upstream-" + weight).weight(weight).build())
                .collect(Collectors.toList());
        int totalWeight = WeightUtil.calculateTotalWeight(upstreams);
        assertEquals(totalWeight, 6);
    }

    @Test
    public void testGetWeight() {
        List<Upstream> upstreams = Stream.of(1, 2, 3)
                .map(weight -> Upstream.builder().url("upstream-" + weight).weight(weight).build())
                .collect(Collectors.toList());
        assertEquals(WeightUtil.getWeight(upstreams.get(0)), 1);
        assertEquals(WeightUtil.getWeight(upstreams.get(1)), 2);
        assertEquals(WeightUtil.getWeight(upstreams.get(2)), 3);
    }
}
