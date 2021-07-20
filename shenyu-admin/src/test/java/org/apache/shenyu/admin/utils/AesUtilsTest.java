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

package org.apache.shenyu.admin.utils;

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for AesUtils.
 */
public final class AesUtilsTest {

    private static final String AES_KEY = "2095132720951327";

    private static final String IV = "6075877187097700";

    @Test
    public void testAesEncryption() {
        assertThat(AesUtils.aesEncryption("123456", AES_KEY, IV), is("bbiB8zbUo3z3oA0VqEB/IA=="));
    }

    @Test
    public void testAesEncryptionForNull() {
        assertThat(AesUtils.aesEncryption(null, AES_KEY, IV), nullValue());
    }

    @Test
    public void testAesDecryption() {
        assertThat(AesUtils.aesDecryption("bbiB8zbUo3z3oA0VqEB/IA==", AES_KEY, IV), is("123456"));
    }

    @Test
    public void testAesDecryptionForEmptyString() {
        assertThat(AesUtils.aesDecryption("", AES_KEY, IV), nullValue());
    }

    @Test
    public void testAesDecryptionForNull() {
        assertThat(AesUtils.aesDecryption(null, AES_KEY, IV), nullValue());
    }
}
