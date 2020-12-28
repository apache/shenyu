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

import org.dromara.soul.common.exception.SoulException;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.emptyString;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for CipherUtils.
 *
 * @author HoldDie
 */
public final class CipherUtilsTest {

    private static final String AES_KEY = "2095132720951327";

    @Test
    public void testEncryptHex() {
        assertThat(CipherUtils.encryptHex("123456", AES_KEY), is("jHcpKkiDbbQh7W7hh8yQSA=="));
    }

    @Test
    public void testEncryptHexForNull() {
        assertThat(CipherUtils.encryptHex("", AES_KEY), emptyString());
    }

    @Test
    public void testDecryptStr() {
        assertThat(CipherUtils.decryptStr("jHcpKkiDbbQh7W7hh8yQSA==", AES_KEY), is("123456"));
    }

    @Test(expected = SoulException.class)
    public void testDecryptStrForErrorStringThrowsException() {
        assertThat(CipherUtils.decryptStr("jHcpKkiDbbQh7W7hh8yQSA=", AES_KEY), notNullValue());
    }

    @Test(expected = SoulException.class)
    public void testDecryptStrForEmptyStringThrowsException() {
        assertThat(CipherUtils.decryptStr("", AES_KEY), notNullValue());
    }

    @Test(expected = AssertionError.class)
    public void testDecryptStrForNullThrowsException() {
        assertThat(CipherUtils.decryptStr(null, AES_KEY), notNullValue());
    }
}

