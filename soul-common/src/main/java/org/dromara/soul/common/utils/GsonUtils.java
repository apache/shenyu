/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Type;
import java.net.URLDecoder;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * GSONUtils.
 *
 * @author xiaoyu(Myth)
 */
public class GsonUtils {

    private static final GsonUtils INSTANCE = new GsonUtils();

    /**
     * The constant STRING.
     */
    private static final TypeAdapter<String> STRING = new TypeAdapter<String>() {

        @Override
        public void write(final JsonWriter out, final String value) {
            try {
                if (StringUtils.isBlank(value)) {
                    out.nullValue();
                    return;
                }
                out.value(value);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        @Override
        public String read(final JsonReader reader) {
            try {
                if (reader.peek() == JsonToken.NULL) {
                    reader.nextNull();
                    // 原先是返回null，这里改为返回空字符串
                    return "";
                }
                return reader.nextString();
            } catch (Exception e) {
                e.printStackTrace();
            }
            return "";
        }

    };

    private static final Gson GSON = new GsonBuilder()
            .registerTypeAdapter(String.class, STRING)
            .create();

    private static final Gson GSON_MAP = new GsonBuilder().serializeNulls().registerTypeHierarchyAdapter(new TypeToken<Map<String, Object>>() {
    }.getRawType(), new MapDeserializer<String, Object>()).create();

    private static final String DOT = ".";

    private static final String E = "e";

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static GsonUtils getInstance() {
        return INSTANCE;
    }

    /**
     * To json string.
     *
     * @param object the object
     * @return the string
     */
    public String toJson(final Object object) {
        return GSON.toJson(object);
    }

    /**
     * From json t.
     *
     * @param <T>    the type parameter
     * @param json   the json
     * @param tClass the t class
     * @return the t
     */
    public <T> T fromJson(final String json, final Class<T> tClass) {
        return GSON.fromJson(json, tClass);
    }


    /**
     * From list list.
     *
     * @param <T>   the type parameter
     * @param json  the json
     * @param clazz the clazz
     * @return the list
     */
    public <T> List<T> fromList(final String json, final Class<T> clazz) {
        return GSON.fromJson(json, TypeToken.getParameterized(List.class, clazz).getType());
    }


    /**
     * toGetParam.
     *
     * @param json json
     * @return java.lang.String string
     */
    public String toGetParam(final String json) {
        if (StringUtils.isBlank(json)) {
            return "";
        }
        final Map<String, String> map = toStringMap(json);
        StringBuilder stringBuilder = new StringBuilder();
        map.forEach((k, v) -> {
            try {
                stringBuilder.append(k)
                        .append("=")
                        .append(URLDecoder.decode(v, Constants.DECODE))
                        .append("&");
            } catch (UnsupportedEncodingException e) {
                e.printStackTrace();
            }
        });
        final String r = stringBuilder.toString();
        return r.substring(0, r.lastIndexOf("&"));

    }

    /**
     * toMap.
     *
     * @param json json
     * @return hashMap map
     */
    private Map<String, String> toStringMap(final String json) {
        return GSON.fromJson(json, new TypeToken<Map<String, String>>() {
        }.getType());
    }


    /**
     * toList Map.
     *
     * @param json json
     * @return hashMap list
     */
    public List<Map> toListMap(final String json) {
        return GSON.fromJson(json, new TypeToken<List<Map>>() {
        }.getType());
    }

    /**
     * To object map map.
     *
     * @param json the json
     * @return the map
     */
    public Map<String, Object> toObjectMap(final String json) {
        return GSON_MAP.fromJson(json, new TypeToken<Map<String, Object>>() {
        }.getType());
    }

    private static class MapDeserializer<T, U> implements JsonDeserializer<Map<T, U>> {

        @Override
        public Map<T, U> deserialize(final JsonElement json, final Type type, final JsonDeserializationContext context) throws JsonParseException {
            if (!json.isJsonObject()) {
                return null;
            }

            JsonObject jsonObject = json.getAsJsonObject();
            Set<Map.Entry<String, JsonElement>> jsonEntrySet = jsonObject.entrySet();
            Map<T, U> resultMap = new LinkedHashMap<>();

            for (Map.Entry<String, JsonElement> entry : jsonEntrySet) {
                U value = context.deserialize(entry.getValue(), this.getType(entry.getValue()));
                resultMap.put((T) entry.getKey(), value);
            }

            return resultMap;
        }

        /**
         * Get JsonElement class type.
         *
         * @param element the element
         * @return Class class
         */
        public Class getType(final JsonElement element) {
            if (element.isJsonPrimitive()) {
                final JsonPrimitive primitive = element.getAsJsonPrimitive();
                if (primitive.isString()) {
                    return String.class;
                } else if (primitive.isNumber()) {
                    String numStr = primitive.getAsString();
                    if (numStr.contains(DOT) || numStr.contains(E)
                            || numStr.contains("E")) {
                        return Double.class;
                    }
                    return Long.class;
                } else if (primitive.isBoolean()) {
                    return Boolean.class;
                }
            }
            return element.getClass();
        }
    }

}
