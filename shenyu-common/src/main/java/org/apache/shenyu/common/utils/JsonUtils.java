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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.type.MapType;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalTimeSerializer;
import java.util.HashMap;
import org.apache.shenyu.common.constant.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * JsonUtils.
 */
public final class JsonUtils {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(JsonUtils.class);

    private static final ObjectMapper MAPPER = new ObjectMapper();

    static {
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(LocalDateTime.class, new LocalDateTimeSerializer(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        javaTimeModule.addSerializer(LocalDate.class, new LocalDateSerializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        javaTimeModule.addSerializer(LocalTime.class, new LocalTimeSerializer(DateTimeFormatter.ofPattern("HH:mm:ss")));
        javaTimeModule.addDeserializer(LocalDateTime.class, new LocalDateTimeDeserializer(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        javaTimeModule.addDeserializer(LocalDate.class, new LocalDateDeserializer(DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        javaTimeModule.addDeserializer(LocalTime.class, new LocalTimeDeserializer(DateTimeFormatter.ofPattern("HH:mm:ss")));
        MAPPER.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
            .disable(SerializationFeature.FAIL_ON_EMPTY_BEANS)
            .configure(JsonParser.Feature.ALLOW_COMMENTS, true)
            .configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
            .configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true)
            .configure(JsonReadFeature.ALLOW_UNESCAPED_CONTROL_CHARS.mappedFeature(), true)
            .setDateFormat(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))
            .registerModule(javaTimeModule)
            .addMixIn(Map.class, IgnoreType.class);
    }

    /**
     * To json string.
     *
     * @param object the object
     * @return the string
     */
    public static String toJson(final Object object) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (IOException e) {
            LOG.warn("write to json string error: " + object, e);
            return Constants.EMPTY_JSON;
        }
    }

    /**
     * Object to Map.
     *
     * @param object the object
     * @return the converted map
     */
    public static Map<String, Object> toMap(final Object object) {
        try {
            String json = MAPPER.writeValueAsString(object);
            final MapType mapType = MAPPER.getTypeFactory().constructMapType(LinkedHashMap.class, String.class, Object.class);
            return MAPPER.readValue(json, mapType);
        } catch (IOException e) {
            LOG.warn("write to map error: " + object, e);
            return new LinkedHashMap<>();
        }
    }

    /**
     * String to Map.
     *
     * @param json         the object
     * @param valueTypeRef class
     * @param <T>          generic
     * @return the converted map
     */
    public static <T> Map<String, T> jsonToMap(final String json, final Class<T> valueTypeRef) {
        try {
            JavaType t = MAPPER.getTypeFactory().constructParametricType(HashMap.class, String.class, valueTypeRef);
            return MAPPER.readValue(json, t);
        } catch (IOException e) {
            LOG.warn("write to map error: " + json, e);
            return new LinkedHashMap<>();
        }
    }

    /**
     * String to Map.
     *
     * @param json the object
     * @return the converted map
     */
    public static Map<String, Object> jsonToMap(final String json) {
        try {
            final MapType mapType = MAPPER.getTypeFactory().constructMapType(LinkedHashMap.class, String.class, Object.class);
            return MAPPER.readValue(json, mapType);
        } catch (IOException e) {
            LOG.warn("write to map error: " + json, e);
            return new LinkedHashMap<>();
        }
    }

    /**
     * Remove class object.
     *
     * @param object the object
     * @return the object
     */
    public static Object removeClass(final Object object) {
        if (object instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) object;
            Object result = map.get("result");
            if (result instanceof Map) {
                Map<?, ?> resultMap = (Map<?, ?>) result;
                resultMap.remove("class");
            }
            map.remove("class");
        }
        return object;
    }

    @JsonIgnoreProperties("class")
    @interface IgnoreType {
    }
}
