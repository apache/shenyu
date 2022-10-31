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
import static org.hamcrest.core.Is.is;

/**
 * Test case for DubboUpstream.
 */
public class DubboUpstreamTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        DubboUpstream upstream = DubboUpstream.builder().protocol("protocol").upstreamHost("host").upstreamUrl("url")
                .status(true).warmup(50).timestamp(1650549243L).weight(100).build();
        
        upstream.setRegistry("reg");
        upstream.setAppName("appName");
        upstream.setPort(9080);
        upstream.setGray(true);
        upstream.setGroup("group");
        upstream.setVersion("version");
        upstream.setWeight(50);
        upstream.setWarmup(100);
        
        assertThat(upstream.getRegistry(), is("reg"));
        assertThat(upstream.getAppName(), is("appName"));
        assertThat(upstream.getPort(), is(9080));
        assertThat(upstream.isGray(), is(true));
        assertThat(upstream.getGroup(), is("group"));
        assertThat(upstream.getVersion(), is("version"));
        assertThat(upstream.getWeight(), is(50));
        assertThat(upstream.getWarmup(), is(100));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        DubboUpstream upstream1 = DubboUpstream.builder().protocol("protocol").upstreamHost("host").upstreamUrl("url")
                .status(true).warmup(50).timestamp(1650549243L).weight(100).build();
        DubboUpstream upstream2 = DubboUpstream.builder().protocol("protocol").upstreamHost("host").upstreamUrl("url")
                .status(true).warmup(50).timestamp(1650549243L).weight(100).build();
        
        assertThat(ImmutableSet.of(upstream1, upstream2), hasSize(1));
    }
    
}
