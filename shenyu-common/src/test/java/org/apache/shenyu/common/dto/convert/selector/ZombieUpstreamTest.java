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

package org.apache.shenyu.common.dto.convert.selector;

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;

/**
 * Test case for ZombieUpstream.
 */
public class ZombieUpstreamTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        ZombieUpstream upstream = ZombieUpstream.builder().selectorId("id").zombieCheckTimes(10)
                .commonUpstream(new CommonUpstream()).build();
        
        upstream.setSelectorName("newId");
        upstream.setZombieCheckTimes(5);
        upstream.setCommonUpstream(null);
        
        assertThat(upstream.getSelectorId(), is("newId"));
        assertThat(upstream.getZombieCheckTimes(), is(5));
        assertThat(upstream.getCommonUpstream(), is(nullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        CommonUpstream commonUpstream = new CommonUpstream();
        commonUpstream.setProtocol("protocol");
        commonUpstream.setUpstreamHost("host");
        commonUpstream.setUpstreamUrl("url");
        commonUpstream.setStatus(true);
        commonUpstream.setTimestamp(1650549243L);
        
        ZombieUpstream upstream1 = ZombieUpstream.builder().selectorId("id").zombieCheckTimes(10)
                .commonUpstream(commonUpstream).build();
        ZombieUpstream upstream2 = ZombieUpstream.builder().selectorId("id").zombieCheckTimes(10)
                .commonUpstream(commonUpstream).build();
        
        assertThat(ImmutableSet.of(upstream1, upstream2), hasSize(1));
    }
    
    @Test
    public void testTransform() {
        ZombieUpstream upstream = ZombieUpstream.transform(new CommonUpstream(), 10, "id");
        
        assertThat(upstream, is(notNullValue()));
    }

}
