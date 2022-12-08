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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

/**
 * Test cases for SignUtils.
 */
public final class SignUtilsTest {

    @Test
    public void testGenerateMd5Sign() {

        assertThat(SignUtils.sign(SignUtils.SIGN_MD5, "test", "a1b2"),
                is("7aa98f7d67f8e4730e2d1d3902295ce6"));
    }

    @Test
    public void testGeneratesSignWithNullKeyOrNullData() {

        assertThrowsExactly(NullPointerException.class,
            () -> SignUtils.sign(SignUtils.SIGN_HS256, "key", null));

        assertThrowsExactly(NullPointerException.class,
            () -> SignUtils.sign(SignUtils.SIGN_HS256, null, "data"));
    }

    @Test
    public void testGeneratesSignWithUnsupportedAlgorithm() {

        assertThrowsExactly(UnsupportedOperationException.class,
            () -> SignUtils.sign("supported_algorithm", "key", "data"));
    }

    @Test
    public void testGenerateKey() {
        assertNotNull(SignUtils.generateKey());
    }
}
