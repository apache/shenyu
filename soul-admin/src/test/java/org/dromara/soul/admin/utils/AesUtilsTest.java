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

package org.dromara.soul.admin.utils;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test cases for AesUtils.
 *
 * @author dengliming
 */
public class AesUtilsTest {

    private static final String AES_KEY = "2095132720951327";

    @Test
    public void testAesEncryption() {
        assertEquals("jHcpKkiDbbQh7W7hh8yQSA==", AesUtils.aesEncryption("123456", AES_KEY));
    }

    @Test
    public void testAesEncryption_for_null() {
        assertEquals(null, AesUtils.aesEncryption("", AES_KEY));
    }

    @Test
    public void testAesDecryption() {
        assertEquals("123456", AesUtils.aesDecryption("jHcpKkiDbbQh7W7hh8yQSA==", AES_KEY));
    }

    @Test
    public void testAesDecryption_for_null() {
        assertEquals(null, AesUtils.aesDecryption("", AES_KEY));
    }

}
