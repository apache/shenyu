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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for PathMatchUtils.
 */
public final class PathMatchUtilsTest {

    @Test
    public void testPathMatch() {
        // test exact matching
        assertTrue(PathMatchUtils.match("test", "test"));
        assertTrue(PathMatchUtils.match("/test", "/test"));
        // test matching with ?'s
        assertTrue(PathMatchUtils.match("t?st", "test"));
        assertTrue(PathMatchUtils.match("??st", "test"));
        // test matching with *'s
        assertTrue(PathMatchUtils.match("*", "test"));
        assertTrue(PathMatchUtils.match("test*", "test"));
        assertTrue(PathMatchUtils.match("test*", "testTest"));
        assertFalse(PathMatchUtils.match("test*aaa", "testblaaab"));
        // test matching with **'s
        assertTrue(PathMatchUtils.match("/**", "/testing/testing"));
        assertTrue(PathMatchUtils.match("/test/**", "/test/test"));
    }
    
    @Test
    public void testPathPattern() {
        // test matching with *'s
        assertTrue(PathMatchUtils.pathPattern("*", "test"));
        assertTrue(PathMatchUtils.pathPattern("test*", "test"));
        assertTrue(PathMatchUtils.pathPattern("test*", "testTest"));
        assertFalse(PathMatchUtils.pathPattern("test*aaa", "testblaaab"));
        // test matching with **'s
        assertTrue(PathMatchUtils.pathPattern("/**", "/testing/testing"));
        assertTrue(PathMatchUtils.pathPattern("/test/**", "/test/test"));
    }

    @Test
    public void testPathVariableHandle() {
        //test filter PathVariable
        assertTrue(PathMatchUtils.match("{id}/{name}", "/demo/order/path/{id}/{name}".substring("/demo/order/path/{id}/{name}".indexOf("{"))));
        //test filter original param
        assertTrue(PathMatchUtils.match("1/godfje@", "/demo/order/path/1/godfje@".substring("demo/order/path/{id}/{name}".indexOf("{") + 1)));
        //test replaceAll result
        final String realPath = PathMatchUtils.replaceAll("demo/order/path/{id}/{name}",
                "/demo/order/path/{id}/{name}".substring("/demo/order/path/{id}/{name}".indexOf("{")),
                "/demo/order/path/1/godfje@".substring("demo/order/path/{id}/{name}".indexOf("{") + 1));
        assertThat(realPath, is("demo/order/path/1/godfje@"));
    }
}
