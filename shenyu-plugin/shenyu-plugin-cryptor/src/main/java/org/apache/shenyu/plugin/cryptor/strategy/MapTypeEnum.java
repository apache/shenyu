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

package org.apache.shenyu.plugin.cryptor.strategy;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * MapTypeEnum.
 */
public enum MapTypeEnum {

    ALL("all") {
        @Override
        public String map(final String originalBody, final String modifiedBody, final String fieldNames) {
            AtomicInteger initDeep = new AtomicInteger();
            initDeep.set(0);
            JsonElement je = JsonParser.parseString(originalBody);
            JsonElement resultJe = JsonUtil.replaceJsonNode(je,
                    initDeep,
                    modifiedBody,
                    Arrays.asList(fieldNames.split("\\.")));
            return resultJe.toString();
        }
    },
    FIELD("field") {
        @Override
        public String map(final String originalBody, final String modifiedBody, final String fieldNames) {
            return modifiedBody;
        }
    };

    private final String mapType;

    MapTypeEnum(final String mapType) {
        this.mapType = mapType;
    }

    /**
     * map to type string.
     *
     * @param originalBody originalBody
     * @param modifiedBody modifiedBody
     * @param fieldNames   fieldNames
     * @return String
     */
    public abstract String map(String originalBody, String modifiedBody, String fieldNames);

    /**
     * get mapType.
     *
     * @return String
     */
    public String getMapType() {
        return mapType;
    }

    /**
     * get mapType enum.
     *
     * @param mapType mapType
     * @return MapTypeEnum
     */
    public static MapTypeEnum mapType(final String mapType) {
        return Arrays.stream(values())
                .filter(type -> type.getMapType().equalsIgnoreCase(mapType))
                .findFirst()
                .orElse(ALL);
    }
}
