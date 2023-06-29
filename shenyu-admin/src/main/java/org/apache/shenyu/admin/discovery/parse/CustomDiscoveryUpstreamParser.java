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

package org.apache.shenyu.admin.discovery.parse;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * DiscoveryUpstreamParser.
 *
 * <p>
 * You can define a custom map mapper  if your custom upstream doesn't fit
 * like is {"${yourUserName}" : "url"}
 * </p>
 */
public class CustomDiscoveryUpstreamParser implements JsonDeserializer<DiscoveryUpstreamData>, KeyValueParser {

    private static final Logger LOG = LoggerFactory.getLogger(CustomDiscoveryUpstreamParser.class);

    private final Map<String, String> conversion;


    /**
     * CustomDiscoveryUpstreamParser.
     *
     * @param conversion          conversion
     */
    public CustomDiscoveryUpstreamParser(final Map<String, String> conversion) {
        this.conversion = conversion;
    }

    @Override
    public DiscoveryUpstreamData deserialize(final JsonElement jsonElement,
                                             final Type type,
                                             final JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject asJsonObject = jsonElement.getAsJsonObject();
        JsonObject afterJson = new JsonObject();
        for (Map.Entry<String, JsonElement> elementEntry : asJsonObject.entrySet()) {
            String key = elementEntry.getKey();
            if (conversion.containsKey(key)) {
                String transferKey = conversion.get(key);
                afterJson.add(transferKey, elementEntry.getValue());
            } else {
                afterJson.add(key, elementEntry.getValue());
            }
        }
        return GsonUtils.getInstance().fromJson(afterJson, DiscoveryUpstreamData.class);
    }

    @Override
    public List<DiscoveryUpstreamData> parseValue(final String jsonString) {
        if (StringUtils.isBlank(jsonString)) {
            return Collections.emptyList();
        }
        GsonBuilder gsonBuilder = new GsonBuilder().registerTypeAdapter(DiscoveryUpstreamData.class, this);
        Gson gson = gsonBuilder.create();
        return Collections.singletonList(gson.fromJson(jsonString, DiscoveryUpstreamData.class));
    }

}
