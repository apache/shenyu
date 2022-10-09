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

package org.apache.shenyu.plugin.logging.mask.utils;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.Md5Utils;
import org.apache.shenyu.plugin.logging.mask.enums.DataMaskEnums;
import org.apache.shenyu.plugin.logging.mask.matcher.KeyWordMatch;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
public class DataMaskUtilsTest {

    private static final String JSON_TEXT = "{\"id\":\"123\",\"name\":\"jack\"}";

    private KeyWordMatch keyWordMatch;

    @BeforeEach
    public void setup() {
        Set<String> sets = new HashSet<>();
        sets.add("name");
        keyWordMatch = new KeyWordMatch(sets);
    }

    @Test
    public void maskSingleKeyword() {
        String noMaskedData = DataMaskUtils.maskSingleKeyword(false, "name", JSON_TEXT, keyWordMatch,
                DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertEquals(JSON_TEXT, noMaskedData);

        String maskedData = DataMaskUtils.maskSingleKeyword(true, "name", JSON_TEXT, keyWordMatch,
                DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertEquals(Md5Utils.md5(JSON_TEXT), maskedData);
    }

    @Test
    public void maskBody() {
        String noMaskedData = DataMaskUtils.maskBody(false, JSON_TEXT, keyWordMatch, DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertEquals(JSON_TEXT, noMaskedData);

        String maskedData = DataMaskUtils.maskBody(true, JSON_TEXT, keyWordMatch, DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Map<String, String> jsonMap = JsonUtils.jsonToMap(JSON_TEXT, String.class);
        jsonMap.put("name", Md5Utils.md5(jsonMap.get("name")));
        String jsonRet = JsonUtils.toJson(jsonMap);
        Assertions.assertEquals(jsonRet, maskedData);

    }

    @Test
    public void maskList() {
        List<String> list = Arrays.asList("name", "test");
        DataMaskUtils.maskList(false, "name", list, keyWordMatch, DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        Assertions.assertTrue(CollectionUtils.isEqualCollection(list, list));
        DataMaskUtils.maskList(true, "name", list, keyWordMatch, DataMaskEnums.MD5_ENCRYPT.getDataMaskAlg());
        List<String> md5List = Arrays.asList(Md5Utils.md5("name"), Md5Utils.md5("test"));
        Assertions.assertTrue(CollectionUtils.isEqualCollection(md5List, list));
    }
}
