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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.Test;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The type random balance test.
 */
public class RandomLoadBalanceTest {

    @Test
    public void randomLoadBalancesWeightEqualTest() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Method select = AbstractLoadBalancer.class.getDeclaredMethod("select", List.class, String.class);
        Object upstreamOrdered = select.invoke(new RandomLoadBalancer(), Stream.of(10, 10, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalancesWeightZeroTest() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Method select = AbstractLoadBalancer.class.getDeclaredMethod("select", List.class, String.class);
        Object upstreamOrdered = select.invoke(new RandomLoadBalancer(), Stream.of(0, 0, 0)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), "");
        assertNotNull(upstreamOrdered);
    }

    /**
     * random load balance test.
     */
    @Test
    public void randomLoadBalanceOrderedWeightTest() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Method select = AbstractLoadBalancer.class.getDeclaredMethod("select", List.class, String.class);
        Object upstreamOrdered = select.invoke(new RandomLoadBalancer(), Stream.of(10, 40, 50)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalanceDisOrderedWeightTest() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Method select = AbstractLoadBalancer.class.getDeclaredMethod("select", List.class, String.class);
        Object upstreamOrdered = select.invoke(new RandomLoadBalancer(), Stream.of(10, 50, 40)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), "");
        assertNotNull(upstreamOrdered);
    }

    @Test
    public void randomLoadBalanceReversedWeightTest() throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        Method select = AbstractLoadBalancer.class.getDeclaredMethod("select", List.class, String.class);
        Object upstreamOrdered = select.invoke(new RandomLoadBalancer(), Stream.of(50, 40, 10)
                .map(weight -> Upstream.builder()
                        .url("upstream-" + weight)
                        .weight(weight)
                        .build())
                .collect(Collectors.toList()), "");
        assertNotNull(upstreamOrdered);
    }
}
