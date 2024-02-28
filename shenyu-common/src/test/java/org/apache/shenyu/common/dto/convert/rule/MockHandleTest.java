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
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for MockHandle.
 */
public class MockHandleTest {

    @Test
    public void testSetterGetter() {
        MockHandle handle = new MockHandle();
        handle.setHttpStatusCode(200);
        handle.setResponseContent("OK");

        assertThat(handle.getHttpStatusCode(), is(200));
        assertThat(handle.getResponseContent(), is("OK"));
    }

    @Test
    public void testEqualsAndHashCode() {
        MockHandle handle1 = new MockHandle();
        MockHandle handle2 = new MockHandle();

        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }

}
