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
import com.google.gson.reflect.TypeToken;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Type;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * GSONUtils.
 *
 * @author xiaoyu(Myth)
 */
@SuppressWarnings("all")
public class GSONUtils {

    private static final GSONUtils INSTANCE = new GSONUtils();

    private static final Gson GSON = new Gson();

    private class MapDeserializer<T, U> implements JsonDeserializer<Map<T, U>> {

        @Override
        public Map<T, U> deserialize(JsonElement json, Type type, JsonDeserializationContext context) throws JsonParseException {
            if (!json.isJsonObject()) {
                return null;
            }

            JsonObject jsonObject = json.getAsJsonObject();
            Set<Map.Entry<String, JsonElement>> jsonEntrySet = jsonObject.entrySet();
            Map<T, U> deserializedMap = new LinkedHashMap<>();

            for (Map.Entry<String, JsonElement> entry : jsonEntrySet) {
                U value = context.deserialize(entry.getValue(), this.getType(entry.getValue()));
                deserializedMap.put((T) entry.getKey(), value);
            }

            return deserializedMap;
        }

        /**
         * Get JsonElement class type.
         *
         * @param element the element
         * @return Class class
         */
        public Class getType(JsonElement element) {
            if (element.isJsonPrimitive()) {
                final JsonPrimitive primitive = element.getAsJsonPrimitive();
                if (primitive.isString()) {
                    return String.class;
                } else if (primitive.isNumber()) {
                    String numStr = primitive.getAsString();
                    if (numStr.contains(".") || numStr.contains("e")
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


    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static GSONUtils getInstance() {
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
        map.forEach((k, v) -> stringBuilder.append(k).append("=").append(v).append("&"));
        final String r = stringBuilder.toString();
        return r.substring(0, r.lastIndexOf("&"));

    }

    /**
     * toMap.
     *
     * @param json json
     * @return hashMap map
     */
    public Map<String, String> toStringMap(final String json) {
        return GSON.fromJson(json, new TypeToken<Map<String, String>>() {
        }.getType());
    }


    /**
     * toList<Map></Map>.
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
        TypeToken typeToken = new TypeToken<Map<String, Object>>() {};
        Gson gson = new GsonBuilder().serializeNulls().registerTypeHierarchyAdapter(typeToken.getRawType(), new MapDeserializer<String, Object>()).create();
        return gson.fromJson(json, typeToken.getType());
    }
}
