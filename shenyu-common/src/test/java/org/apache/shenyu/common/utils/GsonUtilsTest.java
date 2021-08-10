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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.hamcrest.Matchers.comparesEqualTo;
import static org.junit.Assert.assertThat;

/**
 * Test cases for GsonUtils.
 */
public class GsonUtilsTest {

    private static final String EXPECTED_JSON = "{\"bool\":true,\"iNumber\":1,\"fNumber\":1.0,\"dNumber\":1.0,"
            + "\"bigDecimal\":1,\"string\":\"test\",\"testList\":[\"list_string_0\",\"list_string_1\","
            + "\"list_string_2\",\"list_string_3\",\"list_string_4\",\"list_string_5\",\"list_string_6\","
            + "\"list_string_7\",\"list_string_8\",\"list_string_9\"],\"testMap\":{\"map_key_2\":\"map_value_2\","
            + "\"map_key_1\":\"map_value_1\",\"map_key_0\":\"map_value_0\",\"map_key_9\":\"map_value_9\","
            + "\"map_key_8\":\"map_value_8\",\"map_key_7\":\"map_value_7\",\"map_key_6\":\"map_value_6\","
            + "\"map_key_5\":\"map_value_5\",\"map_key_4\":\"map_value_4\",\"map_key_3\":\"map_value_3\"},"
            + "\"emptyList\":[],\"emptyMap\":{},\"nestedMap\":{\"map_2\":{},\"map_1\":{\"map_key_2\":\"map_value_2\","
            + "\"map_key_1\":\"map_value_1\",\"map_key_0\":\"map_value_0\",\"map_key_9\":\"map_value_9\","
            + "\"map_key_8\":\"map_value_8\",\"map_key_7\":\"map_value_7\",\"map_key_6\":\"map_value_6\","
            + "\"map_key_5\":\"map_value_5\",\"map_key_4\":\"map_value_4\",\"map_key_3\":\"map_value_3\"},"
            + "\"bool\":false}}";

    /**
     * test method about {@link GsonUtils#toJson(java.lang.Object)}.
     */
    @Test
    public void testToJson() {
        TestObject testObject = generateTestObject();

        Assert.assertEquals(EXPECTED_JSON, GsonUtils.getInstance().toJson(testObject));
    }

    /**
     * test method about {@link GsonUtils#fromJson(JsonElement, Class)}.
     */
    @Test
    public void testFromJsonAboutJsonElement() {
        TestObject testObject = generateTestObject();

        JsonObject jsonObject = new JsonParser().parse(EXPECTED_JSON).getAsJsonObject();
        TestObject parseObject = GsonUtils.getInstance().fromJson(jsonObject, TestObject.class);

        Assert.assertEquals(testObject, parseObject);
    }

    /**
     * test method about {@link GsonUtils#fromJson(String, Class)}.
     */
    @Test
    public void testFromJsonAboutString() {
        TestObject testObject = generateTestObject();

        Assert.assertEquals(testObject, GsonUtils.getInstance().fromJson(EXPECTED_JSON, TestObject.class));
    }

    /**
     * test method about {@link GsonUtils#fromList(String, Class)}.
     */
    @Test
    public void testFromList() {
        List<String> testList = ImmutableList.of("123", "test", "测试");

        String testJson = "[\"123\",\"test\",\"测试\"]";

        Assert.assertEquals(testList, GsonUtils.getInstance().fromList(testJson, String.class));
    }

    /**
     * test method about {@link GsonUtils#toGetParam(String)}.
     */
    @Test
    public void testToGetParam() {
        Map<String, String> param = ImmutableMap.of("id", "123", "name", "test", "data", "测试");

        String json = GsonUtils.getGson().toJson(param, new TypeToken<Map<String, String>>() {
        }.getType());

        String resultParam = GsonUtils.getInstance().toGetParam(json);
        Map<String, String> resultMap = Arrays.stream(resultParam.split("&"))
                .collect(Collectors.toMap(s -> s.split("=")[0], s -> s.split("=")[1]));

        param.forEach((key, value) -> {
            Assert.assertTrue(resultMap.containsKey(key));
            Assert.assertEquals(value, resultMap.get(key));
        });

        Assert.assertEquals("", GsonUtils.getInstance().toGetParam(""));
    }

    /**
     * test method {@link GsonUtils#toListMap(String)}.
     */
    @Test
    public void testToListMap() {
        Map<String, Object> map = ImmutableMap.of("id", "123", "name", "test", "data", "测试");
        List<Map<String, Object>> list = ImmutableList.of(ImmutableMap.copyOf(map), ImmutableMap.copyOf(map),
                ImmutableMap.copyOf(map));

        String json = "[{\"name\":\"test\",\"id\":\"123\",\"data\":\"测试\"},"
                + "{\"name\":\"test\",\"id\":\"123\",\"data\":\"测试\"},"
                + "{\"name\":\"test\",\"id\":\"123\",\"data\":\"测试\"}]";

        Assert.assertEquals(list, GsonUtils.getInstance().toListMap(json));
    }

    /**
     * test method {@link GsonUtils#toObjectMap(java.lang.String)}.
     */
    @Test
    public void testToObjectMap() {
        Map<String, Object> map = ImmutableMap.of("id", 123L, "name", "test", "double", 1.0D,
                "boolean", true, "data", generateTestObject());

        String json = "{\"name\":\"test\",\"id\":123,\"double\":1.0,\"boolean\":true,\"data\":" + EXPECTED_JSON + "}";

        Map<String, Object> parseMap = GsonUtils.getInstance().toObjectMap(json);
        map.forEach((key, value) -> {
            Assert.assertTrue(parseMap.containsKey(key));
            Object jsonValue = parseMap.get(key);
            if (jsonValue instanceof JsonElement) {
                Assert.assertEquals(value, GsonUtils.getInstance().fromJson((JsonElement) jsonValue, TestObject.class));
            } else {
                Assert.assertEquals(value, parseMap.get(key));
            }
        });

        Assert.assertNull(GsonUtils.getInstance().toObjectMap(null));
    }

    /**
     * test method {@link GsonUtils#toObjectMap(java.lang.String, java.lang.Class)}.
     */
    @Test
    public void testToObjectMapWithClazz() {
        Map<String, TestObject> map = ImmutableMap.of("data", generateTestObject());

        String json = "{\"data\":" + EXPECTED_JSON + "}";

        Map<String, TestObject> parseMap = GsonUtils.getInstance().toObjectMap(json, TestObject.class);
        map.forEach((key, value) -> {
            Assert.assertTrue(parseMap.containsKey(key));
            Assert.assertEquals(value, parseMap.get(key));
        });

        Assert.assertNull(GsonUtils.getInstance().toObjectMap(null, String.class));
    }

    /**
     * test method {@link GsonUtils#toObjectMapList(java.lang.String, java.lang.Class)}.
     */
    @Test
    public void testToObjectMapList() {
        List<String> listFirst = ImmutableList.of("111", "222");
        List<String> listSecond = ImmutableList.of("333", "555");
        Map<String, List<String>> map = ImmutableMap.of("data1", listFirst, "data2", listSecond);

        String json = "{\"data1\":[\"111\",\"222\"],\"data2\":[\"333\",\"555\"]}";
        Map<String, List<String>> parseMap = GsonUtils.getInstance().toObjectMapList(json, String.class);
        map.forEach((key, value) -> {
            Assert.assertTrue(parseMap.containsKey(key));
            Assert.assertEquals(value, parseMap.get(key));
        });

        Assert.assertNull(GsonUtils.getInstance().toObjectMapList(null, String.class));
    }

    /**
     * test method {@link GsonUtils#toTreeMap(String)}.
     */
    @Test
    public void testToTreeMap() {
        Map<String, Object> map = ImmutableMap.of("id", 123L, "name", "test", "double",
                1.0D, "boolean", true, "data", generateTestObject());

        String json = "{\"name\":\"test\",\"id\":123,\"double\":1.0,\"boolean\":true,\"data\":"
                + EXPECTED_JSON + "}";

        Map<String, Object> parseMap = GsonUtils.getInstance().toTreeMap(json);

        map.forEach((key, value) -> {
            Assert.assertTrue(parseMap.containsKey(key));
            Object jsonValue = parseMap.get(key);
            if (jsonValue instanceof JsonElement) {
                Assert.assertEquals(value, GsonUtils.getInstance().fromJson((JsonElement) jsonValue, TestObject.class));
            } else {
                Assert.assertEquals(value, parseMap.get(key));
            }
        });

        Assert.assertNull(GsonUtils.getInstance().toObjectMap(null));
    }

    /**
     * test method {@link GsonUtils#convertToMap(String)}.
     */
    @Test
    public void testConvertToMap() {
        List<Integer> innerList = ImmutableList.of(1, 2, 3);
        Map<String, Object> innerMap = ImmutableMap.of("id", 123, "name", "shenyu");
        Map<String, Object> map = ImmutableMap.of("code", 200, "message", "test",
                "data", innerMap, "list", innerList);

        String testJson = "{\"code\":200,\"message\":\"test\","
                + "\"data\":{\"id\":123,\"name\":\"shenyu\"},\"list\":[1,2,3]}";
        Map<String, Object> parseMap = GsonUtils.getInstance().convertToMap(testJson);

        map.forEach((key, value) -> {
            Assert.assertTrue(parseMap.containsKey(key));
            if (value instanceof Map) {
                Map<?, ?> tempMap = (Map<?, ?>) parseMap.get(key);
                ((Map<?, ?>) value).forEach((key1, value1) -> {
                    Assert.assertTrue(tempMap.containsKey(key1));
                    Assert.assertEquals(value1.toString(), tempMap.get(key1).toString());
                });
            } else if (value instanceof List) {
                List<?> tempList = (List<?>) parseMap.get(key);
                List<?> tempValue = (List<?>) value;
                for (int i = 0; i < tempValue.size(); i++) {
                    Assert.assertEquals(tempValue.get(i).toString(), tempList.get(i).toString());
                }
            } else {
                Assert.assertEquals(value.toString(), parseMap.get(key).toString());
            }
        });

        Assert.assertNull(GsonUtils.getInstance().convertToMap(null));
    }

    @Test
    public void testPairGson() {
        Pair<String, String> testPair = Pair.of("1", "2");

        String testJson = "{\"left\":\"1\",\"right\":\"2\"}";
        Pair<String, String> resultPair = GsonUtils.getInstance().fromJson(testJson, Pair.class);

        String testListJson = "[{\"left\":\"int\",\"right\":\"param1\"},{\"left\":\"java.lang.Integer\",\"right\":\"param2\"}]";
        List<Pair> listPair = GsonUtils.getInstance().fromList(testListJson, Pair.class);
        String resultListJson = GsonUtils.getInstance().toJson(listPair);

        assertThat(resultListJson, comparesEqualTo(testListJson));
        assertThat(resultPair.getLeft(), comparesEqualTo(testPair.getLeft()));
        assertThat(resultPair.getRight(), comparesEqualTo(testPair.getRight()));
    }

    private static TestObject generateTestObject() {
        List<String> testList = Lists.newLinkedList();
        Map<String, String> testMap = Maps.newHashMap();
        for (int i = 0; i < 10; i++) {
            testList.add("list_string_" + i);
            testMap.put("map_key_" + i, "map_value_" + i);
        }

        return TestObject.builder()
                .bool(true)
                .iNumber(1)
                .fNumber(1.0f)
                .dNumber(1.0d)
                .bigDecimal(BigDecimal.ONE)
                .string("test")
                .nullObject(null)
                .testList(testList)
                .testMap(testMap)
                .emptyList(Lists.newLinkedList())
                .emptyMap(Maps.newHashMap())
                .nestedMap(new HashMap<String, Object>() {
                    {
                        put("map_1", testMap);
                        put("map_2", Maps.newHashMap());
                        put("bool", false);
                    }
                })
                .build();
    }

    private static class TestObject {
        private Boolean bool;

        private Integer iNumber;

        private Float fNumber;

        private Double dNumber;

        private BigDecimal bigDecimal;

        private String string;

        private Object nullObject;

        private List<String> testList;

        private Map<String, String> testMap;

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
            this.bool = builder.bool;
            this.iNumber = builder.iNumber;
            this.fNumber = builder.fNumber;
            this.dNumber = builder.dNumber;
            this.bigDecimal = builder.bigDecimal;
            this.string = builder.string;
            this.nullObject = builder.nullObject;
            this.testList = builder.testList;
            this.testMap = builder.testMap;
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
         * get bool.
         *
         * @return bool
         */
        public Boolean getBool() {
            return bool;
        }

        /**
         * set bool.
         *
         * @param bool bool
         */
        public void setBool(final Boolean bool) {
            this.bool = bool;
        }

        /**
         * get iNumber.
         *
         * @return iNumber
         */
        public Integer getiNumber() {
            return iNumber;
        }

        /**
         * set iNumber.
         *
         * @param iNumber iNumber
         */
        public void setiNumber(final Integer iNumber) {
            this.iNumber = iNumber;
        }

        /**
         * get fNumber.
         *
         * @return fNumber
         */
        public Float getfNumber() {
            return fNumber;
        }

        /**
         * set fNumber.
         *
         * @param fNumber fNumber
         */
        public void setfNumber(final Float fNumber) {
            this.fNumber = fNumber;
        }

        /**
         * get dNumber.
         *
         * @return dNumber
         */
        public Double getdNumber() {
            return dNumber;
        }

        /**
         * set dNumber.
         *
         * @param dNumber dNumber
         */
        public void setdNumber(final Double dNumber) {
            this.dNumber = dNumber;
        }

        /**
         * get bigDecimal.
         *
         * @return bigDecimal
         */
        public BigDecimal getBigDecimal() {
            return bigDecimal;
        }

        /**
         * set bigDecimal.
         *
         * @param bigDecimal bigDecimal
         */
        public void setBigDecimal(final BigDecimal bigDecimal) {
            this.bigDecimal = bigDecimal;
        }

        /**
         * get string.
         *
         * @return string
         */
        public String getString() {
            return string;
        }

        /**
         * set string.
         *
         * @param string string
         */
        public void setString(final String string) {
            this.string = string;
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
            return Objects.equals(bool, that.bool) && Objects.equals(iNumber, that.iNumber) && Objects.equals(fNumber, that.fNumber) && Objects.equals(dNumber, that.dNumber)
                    && Objects.equals(bigDecimal, that.bigDecimal) && Objects.equals(string, that.string) && Objects.equals(nullObject, that.nullObject)
                    && Objects.equals(testList, that.testList) && Objects.equals(testMap, that.testMap) && Objects.equals(emptyList, that.emptyList)
                    && Objects.equals(emptyMap, that.emptyMap) && Objects.equals(nestedMap, that.nestedMap);
        }

        @Override
        public int hashCode() {
            return Objects.hash(bool, iNumber, fNumber, dNumber, bigDecimal, string, nullObject, testList, testMap, emptyList, emptyMap, nestedMap);
        }

        @Override
        public String toString() {
            return "TestObject{"
                    + "bool="
                    + bool
                    + ", iNumber="
                    + iNumber
                    + ", fNumber="
                    + fNumber
                    + ", dNumber="
                    + dNumber
                    + ", bigDecimal="
                    + bigDecimal
                    + ", string='"
                    + string
                    + '\''
                    + ", nullObject="
                    + nullObject
                    + ", testList="
                    + testList
                    + ", testMap="
                    + testMap
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

            private Boolean bool;

            private Integer iNumber;

            private Float fNumber;

            private Double dNumber;

            private BigDecimal bigDecimal;

            private String string;

            private Object nullObject;

            private List<String> testList;

            private Map<String, String> testMap;

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
             * build bool.
             *
             * @param bool bool
             * @return this
             */
            public Builder bool(final Boolean bool) {
                this.bool = bool;
                return this;
            }

            /**
             * build iNumber.
             *
             * @param iNumber iNumber
             * @return this
             */
            public Builder iNumber(final Integer iNumber) {
                this.iNumber = iNumber;
                return this;
            }

            /**
             * build fNumber.
             *
             * @param fNumber fNumber
             * @return this
             */
            public Builder fNumber(final Float fNumber) {
                this.fNumber = fNumber;
                return this;
            }

            /**
             * build dNumber.
             *
             * @param dNumber dNumber
             * @return this
             */
            public Builder dNumber(final Double dNumber) {
                this.dNumber = dNumber;
                return this;
            }

            /**
             * return bigDecimal.
             *
             * @param bigDecimal bigDecimal
             * @return this
             */
            public Builder bigDecimal(final BigDecimal bigDecimal) {
                this.bigDecimal = bigDecimal;
                return this;
            }

            /**
             * build string.
             *
             * @param string string
             * @return this
             */
            public Builder string(final String string) {
                this.string = string;
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
