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

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * JsonUtils.
 *
 * @author xiaoyu
 */
public final class JsonUtils {

    private static ObjectMapper mapper = new ObjectMapper();

    private static Logger logger = LoggerFactory.getLogger(JsonUtils.class);

    static {
    /*    JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(LocalDateTime.class, new LocalDateTimeSerializer(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        javaTimeModule.addSerializer(LocalDate.class, new LocalDateSerializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        javaTimeModule.addSerializer(LocalTime.class, new LocalTimeSerializer(DateTimeFormatter.ofPattern("HH:mm:ss")));
        javaTimeModule.addDeserializer(LocalDateTime.class, new LocalDateTimeDeserializer(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        javaTimeModule.addDeserializer(LocalDate.class, new LocalDateDeserializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        javaTimeModule.addDeserializer(LocalTime.class, new LocalTimeDeserializer(DateTimeFormatter.ofPattern("HH:mm:ss")));
        FilterProvider filterProvider = new SimpleFilterProvider()
                .addFilter("classFilter", SimpleBeanPropertyFilter.serializeAllExcept("class"));
        mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
                .configure(JsonParser.Feature.ALLOW_COMMENTS, true)
                .configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
                .configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true)
                .configure(JsonParser.Feature.ALLOW_UNQUOTED_CONTROL_CHARS, true)
                .setDateFormat(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))
                .registerModule(javaTimeModule)
                .setFilterProvider(filterProvider);*/


    }


    /**
     * To json string.
     *
     * @param object the object
     * @return the string
     */
    public static String toJson(Object object) {
        try {
            return mapper.writeValueAsString(object);
        } catch (IOException e) {
            logger.warn("write to json string error:" + object, e);
            return null;
        }
    }

    /**
     * json To map.
     *
     * @param json the json
     * @return the map.
     */
    public static Map toMap(String json) {
        try {
            return mapper.readValue(json, HashMap.class);
        } catch (IOException e) {
            return Collections.emptyMap();
        }
    }

    /**
     * Dubbo result json string.
     *
     * @param object the object
     * @return the string
     */
    public static String dubboResultJson(Object object) {
        try {
            if (object instanceof Map) {
                Map map = (Map) object;
                map.remove("class");
                return mapper.writeValueAsString(map);
            }
            return mapper.writeValueAsString(object);
        } catch (IOException e) {
            logger.warn("write to json string error:" + object, e);
            return null;
        }
    }


}
