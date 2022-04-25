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
 * Test case for RewriteHandle.
 */
public class RewriteHandleTest {
    
    @Test
    public void testGetterSetter() {
        RewriteHandle handle = new RewriteHandle();
        handle.setRegex("regex");
        handle.setReplace("replace");
        
        assertThat(handle.getRegex(), is("regex"));
        assertThat(handle.getReplace(), is("replace"));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        RewriteHandle handle1 = new RewriteHandle();
        RewriteHandle handle2 = new RewriteHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
}
