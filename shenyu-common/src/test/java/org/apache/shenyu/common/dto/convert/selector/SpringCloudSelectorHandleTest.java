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

import java.util.Arrays;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for SpringCloudSelectorHandle.
 */
public class SpringCloudSelectorHandleTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        SpringCloudSelectorHandle selectorHandle = SpringCloudSelectorHandle.builder().serviceId("serviceId")
                .gray(true).divideUpstreams(Arrays.asList(new DivideUpstream())).build();
        
        selectorHandle.setServiceId("serviceId1");
        selectorHandle.setGray(false);
        selectorHandle.setDivideUpstreams(Arrays.asList(new DivideUpstream(), new DivideUpstream()));
        
        assertThat(selectorHandle.getGray(), is(false));
        assertThat(selectorHandle.getServiceId(), is("serviceId1"));
        assertThat(selectorHandle.getDivideUpstreams(), hasSize(2));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        SpringCloudSelectorHandle selectorHandle1 = SpringCloudSelectorHandle.builder().serviceId("serviceId").build();
        SpringCloudSelectorHandle selectorHandle2 = SpringCloudSelectorHandle.builder().serviceId("serviceId").build();
        
        assertThat(ImmutableSet.of(selectorHandle1, selectorHandle2), hasSize(1));
    }
    
}
