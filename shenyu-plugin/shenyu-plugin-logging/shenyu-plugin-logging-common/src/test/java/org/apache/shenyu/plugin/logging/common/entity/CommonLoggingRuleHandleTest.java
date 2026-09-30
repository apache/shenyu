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

package org.apache.shenyu.plugin.logging.common.entity;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

/**
 * Test case for CommonLoggingRuleHandle.
 */
public class CommonLoggingRuleHandleTest {

    @Test
    public void testSetMaskTypeRoundTrip() {
        CommonLoggingRuleHandle handle = new CommonLoggingRuleHandle();

        // setMaskType used to assign the field to itself and silently drop the value
        handle.setMaskType("mask");
        assertThat(handle.getMaskType(), is("mask"));

        handle.setMaskType(null);
        assertThat(handle.getMaskType(), is((String) null));
    }
}
