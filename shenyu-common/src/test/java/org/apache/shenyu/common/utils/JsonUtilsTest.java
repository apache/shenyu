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

import lombok.Builder;
import lombok.Data;
import org.junit.Test;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;

/**
 * Test cases for JsonUtils.
 */
public final class JsonUtilsTest {

    private static final String EXPECTED_JSON = "{\"id\":123,\"name\":\"test object\",\"deleted\":false,"
             + "\"testList\":[\"test_string_0\",\"test_string_1\",\"test_string_2\",\"test_string_3\",\"test_string_4\",\"test_string_5\","
             + "\"test_string_6\",\"test_string_7\",\"test_string_8\",\"test_string_9\"],\"testMap\""
             + ":{\"test_map_9\":\"test_value_9\",\"test_map_8\":\"test_value_8\",\"test_map_3\":\"test_value_3\","
             + "\"test_map_2\":\"test_value_2\",\"test_map_1\":\"test_value_1\",\"test_map_0\":\"test_value_0\","
             + "\"test_map_7\":\"test_value_7\",\"test_map_6\":\"test_value_6\",\"test_map_5\":\"test_value_5\",\"test_map_4\":\"test_value_4\"},"
             + "\"nullObject\":null,\"emptyList\":[],\"emptyMap\":{},\"nestedMap\":{\"boolean\":false,\"map2\":{},\"map1\":{\"test_map_9\":\"test_value_9\","
             + "\"test_map_8\":\"test_value_8\",\"test_map_3\":\"test_value_3\",\"test_map_2\":\"test_value_2\",\"test_map_1\":\"test_value_1\","
             + "\"test_map_0\":\"test_value_0\",\"test_map_7\":\"test_value_7\","
             + "\"test_map_6\":\"test_value_6\",\"test_map_5\":\"test_value_5\",\"test_map_4\":\"test_value_4\"},\"testInt\":100}}";

    @Test
    public void toJson() {
        List<String> testList = new LinkedList<>();
        Map<String, String> testMap = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            testList.add("test_string_" + i);
            testMap.put("test_map_" + i, "test_value_" + i);
        }
        TestObject object = TestObject.builder()
                .id(123)
                .name("test object")
                .deleted(false)
                .testList(testList)
                .testMap(testMap)
                .nullObject(null)
                .emptyList(new LinkedList<>())
                .emptyMap(new HashMap<>())
                .nestedMap(new HashMap<String, Object>() {
                    {
                        put("map1", testMap);
                        put("map2", new HashMap<>());
                        put("boolean", false);
                        put("testInt", 100);
                        put("class", "nestedClass");
                    }
                })
                .build();
        assertEquals(EXPECTED_JSON, JsonUtils.toJson(object));

        Object o = new Object();
        assertEquals("{}", JsonUtils.toJson(o));
    }

    @Test
    public void removeClass() {
        Map<String, Map<String, String>> testMap = new HashMap<>();
        Map<String, String> testSubMap = new HashMap<>();
        testSubMap.put("class", "NullPointerException.class");
        testSubMap.put("not_class", "ClassNotFoundException.class");
        testMap.put("class", testSubMap);
        testMap.put("not_class", testSubMap);
        JsonUtils.removeClass(testMap);
        assertNull(testMap.getOrDefault("class", null));
        assertEquals(testMap.get("not_class").get("not_class"), "ClassNotFoundException.class");

        testMap = new HashMap<>();
        testMap.put("result", testSubMap);
        JsonUtils.removeClass(testMap);
        assertNotNull(testMap.getOrDefault("result", null));
        assertEquals(testMap.get("result").get("not_class"), "ClassNotFoundException.class");
    }

    @Data
    @Builder
    static class TestObject {

        private int id;

        private String name;

        private boolean deleted;

        private List<String> testList;

        private Map<String, String> testMap;

        private Object nullObject;

        private List<String> emptyList;

        private Map<String, String> emptyMap;

        private Map<String, Object> nestedMap;
    }
}
