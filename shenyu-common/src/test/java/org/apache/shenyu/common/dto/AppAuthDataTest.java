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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;

/**
 * Test case for AppAuthData.
 */
public class AppAuthDataTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        AppAuthData appAuthData = AppAuthData.builder().appKey("appKey").appSecret("appSecret").enabled(true)
                .open(true).pathDataList(new ArrayList<>(0)).paramDataList(new ArrayList<>(0))
                .build();
        
        assertThat(appAuthData.getAppKey(), is("appKey"));
        assertThat(appAuthData.getAppSecret(), is("appSecret"));
        assertThat(appAuthData.getEnabled(), is(true));
        assertThat(appAuthData.getOpen(), is(true));
        assertThat(appAuthData.getPathDataList(), notNullValue());
        assertThat(appAuthData.getParamDataList(), notNullValue());
        
        appAuthData.setAppKey("otherAppKey");
        appAuthData.setAppSecret("otherAppSecret");
        appAuthData.setEnabled(false);
        appAuthData.setOpen(false);
        appAuthData.setPathDataList(null);
        appAuthData.setParamDataList(null);
        
        assertThat(appAuthData.getAppKey(), is("otherAppKey"));
        assertThat(appAuthData.getAppSecret(), is("otherAppSecret"));
        assertThat(appAuthData.getEnabled(), is(false));
        assertThat(appAuthData.getOpen(), is(false));
        assertThat(appAuthData.getPathDataList(), nullValue());
        assertThat(appAuthData.getParamDataList(), nullValue());
    }
    
    @Test
    public void testEqualsAndHashCode() {
        AppAuthData appAuthData1 = AppAuthData.builder().appKey("appKey").appSecret("appSecret").enabled(true)
                .open(true).pathDataList(new ArrayList<>(0)).paramDataList(new ArrayList<>(0))
                .build();
        AppAuthData appAuthData2 = AppAuthData.builder().appKey("appKey").appSecret("appSecret").enabled(true)
                .open(true).pathDataList(new ArrayList<>(0)).paramDataList(new ArrayList<>(0))
                .build();
        
        Set<AppAuthData> set = new HashSet<>();
        set.add(appAuthData1);
        set.add(appAuthData2);
        
        assertThat(set, hasSize(1));
    }
    
}
