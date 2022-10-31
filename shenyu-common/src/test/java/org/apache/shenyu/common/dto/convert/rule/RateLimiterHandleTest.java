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

package org.apache.shenyu.common.dto.convert.rule;

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for RateLimiterHandle.
 */
public class RateLimiterHandleTest {
    
    @Test
    public void testGetterSetter() {
        RateLimiterHandle handle = new RateLimiterHandle();
        handle.setAlgorithmName("algorithmName");
        handle.setReplenishRate(500);
        handle.setBurstCapacity(1000);
        handle.setRequestCount(2.0);
        handle.setLoged(true);
        handle.setKeyResolverName("resolverName");
        
        assertThat(handle.getAlgorithmName(), is("algorithmName"));
        assertThat(handle.getReplenishRate(), closeTo(500, 0.1));
        assertThat(handle.getBurstCapacity(), closeTo(1000, 0.1));
        assertThat(handle.getRequestCount(), closeTo(2.0, 0.1));
        assertThat(handle.isLoged(), is(true));
        assertThat(handle.getKeyResolverName(), is("resolverName"));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        RateLimiterHandle handle1 = new RateLimiterHandle();
        RateLimiterHandle handle2 = new RateLimiterHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
}
