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

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;
import org.mockito.internal.util.collections.Sets;

import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;

/**
 * Test case for RequestHandle.
 */
public class RequestHandleTest {
    
    @Test
    public void testGetterSetter() {
        RequestHandle handle = new RequestHandle();
        handle.setHeader(handle.new ShenyuRequestHeader());
        handle.setParameter(handle.new ShenyuRequestParameter());
        handle.setCookie(handle.new ShenyuCookie());
        
        assertThat(handle.getHeader(), is(notNullValue()));
        assertThat(handle.getParameter(), is(notNullValue()));
        assertThat(handle.getCookie(), is(notNullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        RequestHandle handle1 = new RequestHandle();
        RequestHandle handle2 = new RequestHandle();
        
        Set<RequestHandle> set = new HashSet<>();
        set.add(handle1);
        set.add(handle2);
        
        assertThat(set, hasSize(1));
    }
    
    @Test
    public void testIsEmptyConfig() {
        RequestHandle handle = new RequestHandle();
        handle.setHeader(handle.new ShenyuRequestHeader());
        handle.setParameter(handle.new ShenyuRequestParameter());
        handle.setCookie(handle.new ShenyuCookie());
        
        assertThat(handle.isEmptyConfig(), is(true));
    }
    
    @Test
    public void testShenyuRequestHeader() {
        RequestHandle handle = new RequestHandle();
        RequestHandle.ShenyuRequestHeader header = handle.new ShenyuRequestHeader(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );
        
        assertThat(header.isNotEmptyConfig(), is(true));
        assertThat(header.getAddHeaders(), hasEntry("addKey", "addValue"));
        assertThat(header.getReplaceHeaderKeys(), hasEntry("replaceKey", "newKey"));
        assertThat(header.getSetHeaders(), hasEntry("setKey", "newValue"));
        assertThat(header.getRemoveHeaderKeys(), hasItems("removeKey"));
        
        RequestHandle.ShenyuRequestHeader header1 = handle.new ShenyuRequestHeader();
        header1.setAddHeaders(ImmutableMap.of("addKey", "addValue"));
        header1.setReplaceHeaderKeys(ImmutableMap.of("replaceKey", "newKey"));
        header1.setSetHeaders(ImmutableMap.of("setKey", "newValue"));
        header1.setRemoveHeaderKeys(ImmutableSet.of("removeKey"));
        
        assertThat(ImmutableSet.of(header, header1), hasSize(1));
    }
    
    @Test
    public void testShenyuRequestParameter() {
        RequestHandle handle = new RequestHandle();
        RequestHandle.ShenyuRequestParameter parameter = handle.new ShenyuRequestParameter(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );
    
        assertThat(parameter.isNotEmptyConfig(), is(true));
        assertThat(parameter.getAddParameters(), hasEntry("addKey", "addValue"));
        assertThat(parameter.getReplaceParameterKeys(), hasEntry("replaceKey", "newKey"));
        assertThat(parameter.getSetParameters(), hasEntry("setKey", "newValue"));
        assertThat(parameter.getRemoveParameterKeys(), hasItems("removeKey"));
        
        RequestHandle.ShenyuRequestParameter parameter1 = handle.new ShenyuRequestParameter();
        parameter1.setAddParameters(ImmutableMap.of("addKey", "addValue"));
        parameter1.setReplaceParameterKeys(ImmutableMap.of("replaceKey", "newKey"));
        parameter1.setSetParameters(ImmutableMap.of("setKey", "newValue"));
        parameter1.setRemoveParameterKeys(ImmutableSet.of("removeKey"));
        
        assertThat(ImmutableSet.of(parameter, parameter1), hasSize(1));
    }
    
    @Test
    public void testShenyuCookie() {
        RequestHandle handle = new RequestHandle();
        RequestHandle.ShenyuCookie cookie = handle.new ShenyuCookie(
                ImmutableMap.of("addKey", "addValue"), ImmutableMap.of("replaceKey", "newKey"),
                ImmutableMap.of("setKey", "newValue"), Sets.newSet("removeKey")
        );
    
        assertThat(cookie.isNotEmptyConfig(), is(true));
        assertThat(cookie.getAddCookies(), hasEntry("addKey", "addValue"));
        assertThat(cookie.getReplaceCookieKeys(), hasEntry("replaceKey", "newKey"));
        assertThat(cookie.getSetCookies(), hasEntry("setKey", "newValue"));
        assertThat(cookie.getRemoveCookieKeys(), hasItems("removeKey"));
        
        RequestHandle.ShenyuCookie cookie1 = handle.new ShenyuCookie();
        cookie1.setAddCookies(ImmutableMap.of("addKey", "addValue"));
        cookie1.setReplaceCookieKeys(ImmutableMap.of("replaceKey", "newKey"));
        cookie1.setSetCookies(ImmutableMap.of("setKey", "newValue"));
        cookie1.setRemoveCookieKeys(ImmutableSet.of("removeKey"));
        
        assertThat(ImmutableSet.of(cookie, cookie1), hasSize(1));
    }
    
}
