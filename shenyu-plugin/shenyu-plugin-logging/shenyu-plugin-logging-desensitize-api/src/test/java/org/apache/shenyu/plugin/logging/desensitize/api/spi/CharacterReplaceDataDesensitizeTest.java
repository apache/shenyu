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

package org.apache.shenyu.plugin.logging.desensitize.api.spi;

import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.factory.DataDesensitizeFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class CharacterReplaceDataDesensitizeTest {

    @Test
    void doDesensitizeTest() {
        CharacterReplaceDataDesensitize characterReplaceDataMask = new CharacterReplaceDataDesensitize();
        String ret = characterReplaceDataMask.doDesensitize("1");
        Assertions.assertEquals("*", ret);

        String sourceData = "123456789";
        String replaceText = DataDesensitizeFactory.selectDesensitize(sourceData, DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg());
        int maskNum = 0;
        for (char c : replaceText.toCharArray()) {
            if (c == '*') {
                maskNum++;
            }
        }
        Assertions.assertEquals(sourceData.length() / 2, maskNum);
    }
}
