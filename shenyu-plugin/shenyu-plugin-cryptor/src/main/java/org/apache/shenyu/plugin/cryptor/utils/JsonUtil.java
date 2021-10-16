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

package org.apache.shenyu.plugin.cryptor.utils;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.cryptor.dto.CryptorRuleHandle;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * json util.
 */
public class JsonUtil {

    private static String errorCollector;

    /**
     * parser JSON.
     * @param json json Map
     * @param fieldNames params
     * @return str.
     */
    public static String parser(final String json, final String fieldNames) {
        Map<String, Object> map = GsonUtils.getInstance().toObjectMap(json);
        String str = null;
        if (fieldNames.contains(".")) {
            String[] split = fieldNames.split("\\.");
            JsonObject jsonObject = (JsonObject) map.get(split[0]);
            for (int i = 1; i < split.length; i++) {
                if (i == split.length - 1) {
                    str = jsonObject.getAsJsonPrimitive(split[i]).getAsString();
                } else {
                    jsonObject = jsonObject.getAsJsonObject(split[i]);
                }
            }
        } else {
            return map.get(fieldNames) == null ? null : map.get(fieldNames).toString();
        }
        return str;
    }

    /**
     * check param.
     * @param ruleHandle ruleHandle
     * @return is null
     */
    public static boolean checkParam(final CryptorRuleHandle ruleHandle) {
        String json = GsonUtils.getGson().toJson(ruleHandle);
        Map<String, String> map = GsonUtils.getInstance().toObjectMap(json, String.class);
        for (Map.Entry<String, String> entry : map.entrySet()) {
            if (StringUtils.isEmpty(map.get(entry.getKey()))) {
                errorCollector = entry.getKey();
                return true;
            }
        }
        return false;
    }

    /**
     * get error param.
     * @return error param.
     */
    public static String getErrorCollector() {
        return errorCollector;
    }

    /**
     * operate json.
     * @param jsonElement jsonElement
     * @param initDeep default 0
     * @param value The value that needs to be modified
     * @param deepKey json link
     * @return JsonElement
     */
    public static JsonElement replaceJsonNode(final JsonElement jsonElement,
                                              final AtomicInteger initDeep,
                                              final String value,
                                              final List<String> deepKey) {
        if (deepKey.size() == 0) {
            return jsonElement;
        }
        if (jsonElement.isJsonPrimitive()) {
            return jsonElement;
        }
        if (jsonElement.isJsonArray()) {
            JsonArray jsonArray = jsonElement.getAsJsonArray();
            JsonArray jsonArrayNew = new JsonArray();
            for (JsonElement element : jsonArray) {
                jsonArrayNew.add(replaceJsonNode(element, initDeep, value, deepKey));
            }
            return jsonArrayNew;
        }

        if (jsonElement.isJsonObject()) {
            JsonObject object = jsonElement.getAsJsonObject();
            JsonObject objectNew = new JsonObject();
            for (Map.Entry<String, JsonElement> entry : object.entrySet()) {
                if (deepKey.get(initDeep.get()).equals(entry.getKey())) {
                    initDeep.incrementAndGet();
                }
                String key = entry.getKey();
                if (initDeep.get() == deepKey.size()) {
                    initDeep.set(deepKey.size() - 1);
                    object.addProperty(key, value);
                }
                JsonElement jsonEle = object.get(key);
                JsonElement jsonElementNew = replaceJsonNode(jsonEle, initDeep, value, deepKey);
                objectNew.add(key, jsonElementNew);
            }
            return objectNew;
        }
        return jsonElement;
    }
}
