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

package org.apache.shenyu.common.dto;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for AuthPathData.
 */
public class AuthPathDataTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        AuthPathData authPathData = AuthPathData.builder().appName("appName").enabled(true).path("path").build();
        
        assertThat(authPathData.getAppName(), is("appName"));
        assertThat(authPathData.getEnabled(), is(true));
        assertThat(authPathData.getPath(), is("path"));
    
        authPathData.setAppName("otherAppName");
        authPathData.setPath("otherPath");
        authPathData.setEnabled(false);
        
        assertThat(authPathData.getAppName(), is("otherAppName"));
        assertThat(authPathData.getEnabled(), is(false));
        assertThat(authPathData.getPath(), is("otherPath"));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        AuthPathData authPathData1 = AuthPathData.builder().appName("appName").enabled(true).path("path").build();
        AuthPathData authPathData2 = AuthPathData.builder().appName("appName").enabled(true).path("path").build();
    
        Set<AuthPathData> set = new HashSet<>();
        set.add(authPathData1);
        set.add(authPathData2);
        
        assertThat(set, hasSize(1));
    }
    
}
