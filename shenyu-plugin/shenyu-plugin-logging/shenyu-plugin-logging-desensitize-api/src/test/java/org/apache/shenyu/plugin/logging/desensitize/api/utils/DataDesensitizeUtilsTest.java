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

package org.apache.shenyu.plugin.logging.desensitize.api.utils;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
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
public class DataDesensitizeUtilsTest {

    private static final String JSON_TEXT = "{\"id\":\"123\",\"name\":\"jack\"}";

    private KeyWordMatch keyWordMatch;

    @BeforeEach
    public void setup() {
        Set<String> sets = new HashSet<>();
        sets.add("name");
        keyWordMatch = new KeyWordMatch(sets);
    }

    @Test
    public void desensitizeSingleKeywordTest() {
        String noDesensitizedData = DataDesensitizeUtils.desensitizeSingleKeyword(false, "name", JSON_TEXT, keyWordMatch,
                DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        Assertions.assertEquals(JSON_TEXT, noDesensitizedData);

        String desensitizedData = DataDesensitizeUtils.desensitizeSingleKeyword(true, "name", JSON_TEXT, keyWordMatch,
                DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        Assertions.assertEquals(DigestUtils.md5Hex(JSON_TEXT), desensitizedData);
    }

    @Test
    public void desensitizeBodyTest() {
        String noDesensitizedData = DataDesensitizeUtils.desensitizeBody(false, JSON_TEXT, keyWordMatch, DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        Assertions.assertEquals(JSON_TEXT, noDesensitizedData);

        String desensitizedData = DataDesensitizeUtils.desensitizeBody(true, JSON_TEXT, keyWordMatch, DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        Map<String, String> jsonMap = JsonUtils.jsonToMap(JSON_TEXT, String.class);
        jsonMap.put("name", DigestUtils.md5Hex(jsonMap.get("name")));
        String jsonRet = JsonUtils.toJson(jsonMap);
        Assertions.assertEquals(jsonRet, desensitizedData);

    }

    @Test
    public void desensitizeListTest() {
        List<String> list = Arrays.asList("name", "test");
        DataDesensitizeUtils.desensitizeList(false, "name", list, keyWordMatch, DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        Assertions.assertTrue(CollectionUtils.isEqualCollection(list, list));
        DataDesensitizeUtils.desensitizeList(true, "name", list, keyWordMatch, DataDesensitizeEnum.MD5_ENCRYPT.getDataDesensitizeAlg());
        List<String> md5List = Arrays.asList(DigestUtils.md5Hex("name"), DigestUtils.md5Hex("test"));
        Assertions.assertTrue(CollectionUtils.isEqualCollection(md5List, list));
    }
}
