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

package org.apache.shenyu.plugin.logging.mask.factory;

import org.apache.shenyu.common.utils.Md5Utils;
import org.apache.shenyu.plugin.logging.mask.enums.DataMaskEnums;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class DataMaskFactoryTest {

    @Test
    public void selectMask() {
        // EMPTY_STRING
        String emptyStr = DataMaskFactory.selectMask("", DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertEquals("", emptyStr);

        // test for md5
        String sourceData = "123456789";
        String maskedData = DataMaskFactory.selectMask(sourceData, DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertEquals(Md5Utils.md5(sourceData), maskedData);

        // test for replacement
        String replaceText = DataMaskFactory.selectMask(sourceData, DataMaskEnums.CHARACTER_REPLACE.getDataMaskAlg());
        String maskData = "123456789";
        int maskNum = 0;
        for (char c : replaceText.toCharArray()) {
            if (c == '*') {
                maskNum++;
            }
        }
        Assertions.assertEquals(maskData.length() / 2, maskNum);
    }
}
