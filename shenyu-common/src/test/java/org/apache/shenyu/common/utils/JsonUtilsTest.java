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

import org.apache.shenyu.common.constant.Constants;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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
        JsonParser parser = new JsonParser();
        JsonElement expectedJson = parser.parse(EXPECTED_JSON);
        JsonElement objectJson = parser.parse(JsonUtils.toJson(object));
        assertEquals(expectedJson, objectJson);

        Object o = new Object();
        assertEquals(Constants.EMPTY_JSON, JsonUtils.toJson(o));
    }

    @Test
    public void testJsonToMap() {
        Map<String, Object> stringObjectMap = JsonUtils.jsonToMap(EXPECTED_JSON);
        assertEquals(stringObjectMap.get("name"), "test object");
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

        /**
         * no args constructor.
         */
        TestObject() {
        }

        /**
         * builder constructor.
         *
         * @param builder builder
         */
        private TestObject(final Builder builder) {
            this.id = builder.id;
            this.name = builder.name;
            this.deleted = builder.deleted;
            this.testList = builder.testList;
            this.testMap = builder.testMap;
            this.nullObject = builder.nullObject;
            this.emptyList = builder.emptyList;
            this.emptyMap = builder.emptyMap;
            this.nestedMap = builder.nestedMap;
        }

        /**
         * class builder.
         *
         * @return Builder
         */
        public static Builder builder() {
            return new Builder();
        }

        /**
         * get id.
         *
         * @return id
         */
        public int getId() {
            return id;
        }

        /**
         * set id.
         *
         * @param id id
         */
        public void setId(final int id) {
            this.id = id;
        }

        /**
         * get name.
         *
         * @return name
         */
        public String getName() {
            return name;
        }

        /**
         * set name.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * get deleted.
         *
         * @return deleted
         */
        public boolean isDeleted() {
            return deleted;
        }

        /**
         * set deleted.
         *
         * @param deleted deleted
         */
        public void setDeleted(final boolean deleted) {
            this.deleted = deleted;
        }

        /**
         * get testList.
         *
         * @return testList
         */
        public List<String> getTestList() {
            return testList;
        }

        /**
         * set testList.
         *
         * @param testList testList
         */
        public void setTestList(final List<String> testList) {
            this.testList = testList;
        }

        /**
         * get testMap.
         *
         * @return testMap
         */
        public Map<String, String> getTestMap() {
            return testMap;
        }

        /**
         * set testMap.
         *
         * @param testMap testMap
         */
        public void setTestMap(final Map<String, String> testMap) {
            this.testMap = testMap;
        }

        /**
         * get nullObject.
         *
         * @return nullObject
         */
        public Object getNullObject() {
            return nullObject;
        }

        /**
         * set nullObject.
         *
         * @param nullObject nullObject
         */
        public void setNullObject(final Object nullObject) {
            this.nullObject = nullObject;
        }

        /**
         * get emptyList.
         *
         * @return emptyList
         */
        public List<String> getEmptyList() {
            return emptyList;
        }

        /**
         * set emptyList.
         *
         * @param emptyList emptyList
         */
        public void setEmptyList(final List<String> emptyList) {
            this.emptyList = emptyList;
        }

        /**
         * get emptyMap.
         *
         * @return emptyMap
         */
        public Map<String, String> getEmptyMap() {
            return emptyMap;
        }

        /**
         * set emptyMap.
         *
         * @param emptyMap emptyMap
         */
        public void setEmptyMap(final Map<String, String> emptyMap) {
            this.emptyMap = emptyMap;
        }

        /**
         * get nestedMap.
         *
         * @return nestedMap
         */
        public Map<String, Object> getNestedMap() {
            return nestedMap;
        }

        /**
         * set nestedMap.
         *
         * @param nestedMap nestedMap
         */
        public void setNestedMap(final Map<String, Object> nestedMap) {
            this.nestedMap = nestedMap;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestObject that = (TestObject) o;
            return id == that.id && deleted == that.deleted && Objects.equals(name, that.name) && Objects.equals(testList, that.testList)
                    && Objects.equals(testMap, that.testMap) && Objects.equals(nullObject, that.nullObject)
                    && Objects.equals(emptyList, that.emptyList) && Objects.equals(emptyMap, that.emptyMap) && Objects.equals(nestedMap, that.nestedMap);
        }

        @Override
        public int hashCode() {
            return Objects.hash(id, name, deleted, testList, testMap, nullObject, emptyList, emptyMap, nestedMap);
        }

        @Override
        public String toString() {
            return "TestObject{"
                    + "id="
                    + id
                    + ", name='"
                    + name
                    + '\''
                    + ", deleted="
                    + deleted
                    + ", testList="
                    + testList
                    + ", testMap="
                    + testMap
                    + ", nullObject="
                    + nullObject
                    + ", emptyList="
                    + emptyList
                    + ", emptyMap="
                    + emptyMap
                    + ", nestedMap="
                    + nestedMap
                    + '}';
        }

        /**
         * class builder.
         */
        public static final class Builder {

            private int id;

            private String name;

            private boolean deleted;

            private List<String> testList;

            private Map<String, String> testMap;

            private Object nullObject;

            private List<String> emptyList;

            private Map<String, String> emptyMap;

            private Map<String, Object> nestedMap;

            /**
             * no args constructor.
             */
            private Builder() {
            }

            /**
             * build new Object.
             *
             * @return TestObject
             */
            public TestObject build() {
                return new TestObject(this);
            }

            /**
             * build id.
             *
             * @param id id
             * @return this
             */
            public Builder id(final int id) {
                this.id = id;
                return this;
            }

            /**
             * build name.
             *
             * @param name name
             * @return this
             */
            public Builder name(final String name) {
                this.name = name;
                return this;
            }

            /**
             * build deleted.
             *
             * @param deleted deleted
             * @return this
             */
            public Builder deleted(final boolean deleted) {
                this.deleted = deleted;
                return this;
            }

            /**
             * build testList.
             *
             * @param testList testList
             * @return this
             */
            public Builder testList(final List<String> testList) {
                this.testList = testList;
                return this;
            }

            /**
             * build testMap.
             *
             * @param testMap testMap
             * @return this
             */
            public Builder testMap(final Map<String, String> testMap) {
                this.testMap = testMap;
                return this;
            }

            /**
             * build nullObject.
             *
             * @param nullObject nullObject
             * @return this
             */
            public Builder nullObject(final Object nullObject) {
                this.nullObject = nullObject;
                return this;
            }

            /**
             * build emptyList.
             *
             * @param emptyList emptyList
             * @return this
             */
            public Builder emptyList(final List<String> emptyList) {
                this.emptyList = emptyList;
                return this;
            }

            /**
             * build emptyMap.
             *
             * @param emptyMap emptyMap
             * @return this
             */
            public Builder emptyMap(final Map<String, String> emptyMap) {
                this.emptyMap = emptyMap;
                return this;
            }

            /**
             * build nestedMap.
             *
             * @param nestedMap nestedMap
             * @return this
             */
            public Builder nestedMap(final Map<String, Object> nestedMap) {
                this.nestedMap = nestedMap;
                return this;
            }
        }
    }
}
