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

package org.apache.shenyu.plugin.logging.common.datamask;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DataMaskByCharReplaceTest {

    private DataMaskByCharReplace dataMaskByCharReplace;

    @BeforeEach
    void setUp() {

        dataMaskByCharReplace = new DataMaskByCharReplace();
    }

    @Test
    void mask() {

        String maskData = "123456789";
        String mask = dataMaskByCharReplace.mask(maskData);
        int maskNum = 0;
        for (char c : mask.toCharArray()) {
            if (c == '*') {
                maskNum++;
            }
        }
        Assertions.assertEquals(maskData.length() / 2, maskNum);
        String nullMask = dataMaskByCharReplace.mask("");
        Assertions.assertEquals("", nullMask);
        String oneMask = dataMaskByCharReplace.mask("1");
        Assertions.assertEquals("*", oneMask);
    }
}
