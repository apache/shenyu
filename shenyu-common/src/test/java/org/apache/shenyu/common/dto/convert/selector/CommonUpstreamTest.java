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
 * Test case for CommonUpstream.
 */
public class CommonUpstreamTest {
    
    @Test
    public void testGetterSetter() {
        CommonUpstream upstream = new CommonUpstream();
        upstream.setProtocol("protocol");
        upstream.setUpstreamHost("host");
        upstream.setUpstreamUrl("url");
        upstream.setStatus(true);
        upstream.setTimestamp(1650549243L);
        
        assertThat(upstream.getProtocol(), is("protocol"));
        assertThat(upstream.getUpstreamHost(), is("host"));
        assertThat(upstream.getUpstreamUrl(), is("url"));
        assertThat(upstream.isStatus(), is(true));
        assertThat(upstream.getTimestamp(), is(1650549243L));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        CommonUpstream upstream1 = new CommonUpstream("protocol", "host", "url", true, System.currentTimeMillis());
        CommonUpstream upstream2 = new CommonUpstream("protocol", "host", "url", true, System.currentTimeMillis());
        
        assertThat(ImmutableSet.of(upstream1, upstream2), hasSize(1));
    }
    
}
