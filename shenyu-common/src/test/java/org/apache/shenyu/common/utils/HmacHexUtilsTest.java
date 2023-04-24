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

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for HmacUtils.
 */
public class HmacHexUtilsTest {

    @Test
    public void testHmacMd5Hex() {
        assertEquals(HmacHexUtils.hmacMd5Hex("testKey", "testValue"), "3024ffb5567372102ca6775cf8140cb1");
    }

    @Test
    public void testHmacSha256Hex() {
        assertEquals(HmacHexUtils.hmacSha256Hex("testKey", "testValue"),
            "c52d1ebe5e779f5b337dc8f515bf594bd44a7007cb3f4ab1f6c5a15149bed793");
    }

    @Test
    public void testHmacSha512Hex() {
        assertEquals(HmacHexUtils.hmacSha512Hex("testKey", "testValue"),
            "99997ffdee76da2f016fe4ee9256c3361c7dc9f1588be5cabeca9e541f8224db00b10260f4885eaaf29edab66574237058d43f5644b47e0fc13e66b89dbcde68");
    }
}
