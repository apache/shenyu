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

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for {@link RewriteHandleUtils}.
 */
public final class RewriteHandleUtilsTest {

    @Test
    public void testApplicableExpressions() {
        assertNull(RewriteHandleUtils.applicabilityProblem(null, null));
        assertNull(RewriteHandleUtils.applicabilityProblem("", "/new"));
        assertNull(RewriteHandleUtils.applicabilityProblem("test", null));
        assertNull(RewriteHandleUtils.applicabilityProblem("(test)", "/new/$1"));
        assertNull(RewriteHandleUtils.applicabilityProblem("/shenyu/test", "/new"));
        assertNull(RewriteHandleUtils.applicabilityProblem("/shenyu/{id}", "/target/{id}"));
    }

    @Test
    public void testMissingGroupReferenceIsReported() {
        final String problem = RewriteHandleUtils.applicabilityProblem("test", "/new/$1");
        assertNotNull(problem);
        assertTrue(problem.startsWith("invalid replacement template"));
    }

    @Test
    public void testBareDollarIsReported() {
        assertNotNull(RewriteHandleUtils.applicabilityProblem("test", "/v1/$$"));
    }

    @Test
    public void testUnclosedGroupIsReported() {
        final String problem = RewriteHandleUtils.applicabilityProblem("(test", "/new");
        assertNotNull(problem);
        assertTrue(problem.startsWith("invalid regular expression"));
    }

    @Test
    public void testTrailingBackslashExpressionIsReported() {
        // appending the empty alternative would turn the trailing backslash into an escaped pipe
        final String problem = RewriteHandleUtils.applicabilityProblem("test\\", "/new");
        assertNotNull(problem);
        assertTrue(problem.startsWith("invalid regular expression"));
    }
}
