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
        assertNull(RewriteHandleUtils.unapplicableReason(null, null));
        assertNull(RewriteHandleUtils.unapplicableReason("", "/new"));
        assertNull(RewriteHandleUtils.unapplicableReason("test", null));
        assertNull(RewriteHandleUtils.unapplicableReason("(test)", "/new/$1"));
        assertNull(RewriteHandleUtils.unapplicableReason("/shenyu/test", "/new"));
        assertNull(RewriteHandleUtils.unapplicableReason("/shenyu/{id}", "/target/{id}"));
    }

    @Test
    public void testReplacementWithMissingGroup() {
        final String reason = RewriteHandleUtils.unapplicableReason("test", "/new/$1");
        assertNotNull(reason);
        assertTrue(reason.contains("No group 1"));
    }

    @Test
    public void testReplacementWithBareDollar() {
        final String reason = RewriteHandleUtils.unapplicableReason("test", "/v1/$$");
        assertNotNull(reason);
    }

    @Test
    public void testInvalidRegularExpression() {
        final String reason = RewriteHandleUtils.unapplicableReason("(test", "/new");
        assertNotNull(reason);
        assertTrue(reason.startsWith("invalid regular expression"));
    }
}
