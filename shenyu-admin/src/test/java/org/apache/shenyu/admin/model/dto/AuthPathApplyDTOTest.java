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

package org.apache.shenyu.admin.model.dto;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * test cast for {@link AuthPathApplyDTO}.
 */
public final class AuthPathApplyDTOTest {

    private AuthPathApplyDTO authPathApplyDTOUnderTest;

    @Before
    public void setUp() throws Exception {
        authPathApplyDTOUnderTest = new AuthPathApplyDTO();
        authPathApplyDTOUnderTest.setPath("/");
        authPathApplyDTOUnderTest.setAppName("shenyu");
    }

    @Test
    public void testHashCode() {
        final int result = authPathApplyDTOUnderTest.hashCode();
        assertEquals(2057813462, result);
    }

    @Test
    public void testToString() {
        final String result = authPathApplyDTOUnderTest.toString();
        assertEquals("AuthPathApplyDTO{appName='shenyu', path='/'}", result);
    }
}
