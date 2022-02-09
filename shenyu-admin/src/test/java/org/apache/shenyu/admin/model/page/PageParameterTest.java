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

package org.apache.shenyu.admin.model.page;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test case for {@link PageParameter}.
 */
public final class PageParameterTest {

    private PageParameter pageParameterUnderTest;

    @BeforeEach
    public void setUp() {
        pageParameterUnderTest = new PageParameter();
        pageParameterUnderTest = new PageParameter(1, 10);
        pageParameterUnderTest = new PageParameter(1, 10, 100);
    }

    @Test
    public void testEquals() {
        assertEquals(pageParameterUnderTest.getCurrentPage(), 1);
        assertEquals(pageParameterUnderTest.getPageSize(), 10);
        assertEquals(pageParameterUnderTest.getNextPage(), 2);
    }

    @Test
    public void testHashCode() {
        final int result = pageParameterUnderTest.hashCode();
        assertEquals(-1633878767, result);
    }

    @Test
    public void testToString() {
        final String result = pageParameterUnderTest.toString();
        assertEquals("PageParameter{currentPage=1, prePage=1, nextPage=2, pageSize=10, offset=0, totalPage=10, totalCount=100}", result);
    }
}
