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

package org.apache.shenyu.plugin.logging.common.entity;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For LZ4CompressData.
 */
public class LZ4CompressDataTest {
    
    private LZ4CompressData lz4CompressData;

    @BeforeEach
    public void setUp() {
        int length = 5;
        byte[] compressedData = new byte[]{'h', 'e', 'l', 'l', 'o'};
        this.lz4CompressData = new LZ4CompressData(length, compressedData);
    }

    @Test
    public void testGetLength() {
        lz4CompressData.setLength(6);
        Assertions.assertEquals(lz4CompressData.getLength(), 6);
    }

    @Test
    public void testGetCompressedData() {
        byte[] bytes = new byte[]{'h', 'e', 'l', 'l', 'o', '!'};
        lz4CompressData.setCompressedData(bytes);
        Assertions.assertEquals(lz4CompressData.getCompressedData(), bytes);
    }
}
