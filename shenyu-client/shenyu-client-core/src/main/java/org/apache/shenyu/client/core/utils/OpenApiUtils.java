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

package org.apache.shenyu.client.core.utils;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * openApiUtils.
 */
public class OpenApiUtils {

    private static final String EVERY_PATH = "**";

    private static final String LEFT_ANGLE_BRACKETS = "{";

    private static final String RIGHT_ANGLE_BRACKETS = "}";

    private static final String[] QUERY_CLASSES = new String[]{"org.springframework.web.bind.annotation.RequestParam", "org.springframework.web.bind.annotation.RequestPart"};


    /**
     * generateDocumentParameters.
     *
     * @param path   the api path
     * @param method the method
     * @return documentParameters
     */
    public static List<Parameter> generateDocumentParameters(final String path, final Method method) {
        ArrayList<Parameter> list = new ArrayList<>();
        Pair<Boolean, Annotation[][]> query = isQuery(method);
        if (query.getLeft()) {
            for (Annotation[] annotations : query.getRight()) {
                if (annotations.length > 0 && isQueryName(annotations[0].annotationType().getName(), QUERY_CLASSES)) {
                    for (Annotation annotation : annotations) {
                        String name = "";
                        boolean required = false;
                        if (StringUtils.equals(QUERY_CLASSES[0], annotation.annotationType().getName())) {
                            RequestParam requestParam = (RequestParam) annotation;
                            name = requestParam.value();
                            required = requestParam.required();
                        }
                        if (StringUtils.equals(QUERY_CLASSES[1], annotation.annotationType().getName())) {
                            RequestPart requestPart = (RequestPart) annotation;
                            name = requestPart.value();
                            required = requestPart.required();
                        }
                        Parameter parameter = new Parameter();
                        parameter.setIn("query");
                        parameter.setRequired(required);
                        parameter.setName(name);
                        parameter.setSchema(new Schema("string", null));
                        list.add(parameter);
                    }
                }
            }
        } else {
            List<String> segments = UrlPathUtils.getSegments(path);
            for (String segment : segments) {
                if (EVERY_PATH.equals(segment)) {
                    Parameter parameter = new Parameter();
                    parameter.setIn("path");
                    parameter.setName(segment);
                    parameter.setRequired(true);
                    parameter.setSchema(new Schema("string", null));
                    list.add(parameter);
                }
                if (segment.startsWith(LEFT_ANGLE_BRACKETS) && segment.endsWith(RIGHT_ANGLE_BRACKETS)) {
                    String name = segment.substring(1, segment.length() - 1);
                    Parameter parameter = new Parameter();
                    parameter.setIn("path");
                    parameter.setName(name);
                    parameter.setRequired(true);
                    parameter.setSchema(new Schema("string", null));
                    list.add(parameter);
                }
            }
        }
        return list;
    }

    private static Pair<Boolean, Annotation[][]> isQuery(final Method method) {
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        for (Annotation[] parameterAnnotation : parameterAnnotations) {
            if (parameterAnnotation.length > 0 && isQueryName(parameterAnnotation[0].annotationType().getName(), QUERY_CLASSES)) {
                return Pair.of(true, parameterAnnotations);
            }
            return Pair.of(false, null);
        }
        return Pair.of(false, null);
    }

    private static boolean isQueryName(final String name, final String[] names) {
        for (String s : names) {
            if (StringUtils.equals(name, s)) {
                return true;
            }
        }
        return false;
    }

    /**
     * generateDocumentResponse.
     *
     * @param path the api path
     * @return documentResponseMap
     */
    public static Map<String, Object> generateDocumentResponse(final String path) {
        ImmutableMap<Object, Object> contentMap = ImmutableMap.builder()
                .put(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE, ImmutableMap.of("schema", ImmutableMap.of("type", "string")))
                .build();
        ImmutableMap<Object, Object> successMap = ImmutableMap.builder()
                .put("description", path)
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> notFoundMap = ImmutableMap.builder()
                .put("description", StringUtils.join("the path [", path, "] not found"))
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> conflictMap = ImmutableMap.builder()
                .put("description", "conflict")
                .put("content", contentMap).build();
        return ImmutableMap.<String, Object>builder()
                .put("200", successMap)
                .put("404", notFoundMap)
                .put("409", conflictMap)
                .build();
    }

    /**
     * Perhaps a better way is to generate API documentation through OpenAPI annotations.
     *
     * @param method the method
     * @return return type
     */
    public static ResponseType parseReturnType(final Method method) {
        Type returnType = method.getGenericReturnType();
        return parseType("ROOT", returnType, 0, new HashMap<>(16));
    }

    private static ResponseType parseType(final String name, final Type type, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        ResponseType responseType = new ResponseType();
        if (StringUtils.isBlank(name)) {
            responseType.setName(type.getTypeName());
        } else {
            responseType.setName(name);
        }
        if (depth > 5) {
            responseType.setType("object");
            return responseType;
        }
        if (type instanceof Class) {
            return parseClass(responseType, (Class<?>) type, depth, typeVariableMap);
        } else if (type instanceof ParameterizedType) {
            return parseParameterizedType(responseType, (ParameterizedType) type, depth, typeVariableMap);
        } else if (type instanceof GenericArrayType) {
            return parseGenericArrayType(responseType, (GenericArrayType) type, depth, typeVariableMap);
        } else if (type instanceof TypeVariable) {
            Type actualType = typeVariableMap.get(type);
            if (Objects.nonNull(actualType)) {
                return parseType(name, actualType, depth, typeVariableMap);
            } else if (((TypeVariable<?>) type).getBounds().length > 0) {
                Type upperBound = ((TypeVariable<?>) type).getBounds()[0];
                return parseType(name, upperBound, depth, typeVariableMap);
            } else {
                responseType.setType("object");
                return responseType;
            }
        } else if (type instanceof WildcardType) {
            responseType.setType("object");
            return responseType;
        } else {
            responseType.setType("object");
            return responseType;
        }
    }

    private static ResponseType parseClass(final ResponseType responseType, final Class<?> clazz, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        if (clazz.isArray()) {
            responseType.setType("array");
            responseType.setRefs(Collections.singletonList(parseType("ITEMS", clazz.getComponentType(), depth + 1, typeVariableMap)));
            return responseType;
        } else if (clazz.isEnum()) {
            responseType.setType("string");
            return responseType;
        } else if (isBooleanType(clazz)) {
            responseType.setType("boolean");
            return responseType;
        } else if (isIntegerType(clazz)) {
            responseType.setType("integer");
            return responseType;
        } else if (isNumberType(clazz)) {
            responseType.setType("number");
            return responseType;
        } else if (isStringType(clazz)) {
            responseType.setType("string");
            return responseType;
        } else if (isDateType(clazz)) {
            responseType.setType("date");
            return responseType;
        } else {
            List<ResponseType> refs = new ArrayList<>();
            for (Field field : clazz.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }
                refs.add(parseType(field.getName(), field.getGenericType(), depth + 1, typeVariableMap));
            }
            responseType.setType("object");
            responseType.setRefs(refs);
            return responseType;
        }
    }

    private static ResponseType parseParameterizedType(final ResponseType responseType, final ParameterizedType type, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        Class<?> rawType = (Class<?>) type.getRawType();
        Type[] actualTypeArguments = type.getActualTypeArguments();
        TypeVariable<?>[] typeVariables = rawType.getTypeParameters();
        Map<TypeVariable<?>, Type> newTypeVariableMap = new HashMap<>(typeVariableMap);
        for (int i = 0; i < typeVariables.length; i++) {
            newTypeVariableMap.put(typeVariables[i], actualTypeArguments[i]);
        }
        if (Collection.class.isAssignableFrom(rawType)) {
            Type actualType = actualTypeArguments[0];
            ResponseType elementParam = parseType("ITEMS", actualType, depth + 1, newTypeVariableMap);
            responseType.setRefs(Collections.singletonList(elementParam));
            responseType.setType("array");
            return responseType;
        } else if (Map.class.isAssignableFrom(rawType)) {
            Type keyType = actualTypeArguments[0];
            Type valueType = actualTypeArguments[1];
            ResponseType keyParam = parseType("key", keyType, depth + 1, newTypeVariableMap);
            ResponseType valueParam = parseType("value", valueType, depth + 1, newTypeVariableMap);
            List<ResponseType> children = new ArrayList<>();
            children.add(keyParam);
            children.add(valueParam);
            responseType.setRefs(children);
            responseType.setType("map");
            return responseType;
        } else {
            List<ResponseType> refs = new ArrayList<>();
            for (Field field : rawType.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }
                ResponseType fieldParam = parseType(field.getName(), field.getGenericType(), depth + 1, newTypeVariableMap);
                refs.add(fieldParam);
            }
            responseType.setType("object");
            responseType.setRefs(refs);
            return responseType;
        }
    }

    private static ResponseType parseGenericArrayType(final ResponseType responseType, final GenericArrayType type, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        responseType.setRefs(Collections.singletonList(parseType("ITEMS", type.getGenericComponentType(), depth + 1, typeVariableMap)));
        responseType.setType("array");
        return responseType;
    }

    private static boolean isDateType(final Class<?> clazz) {
        return clazz == Date.class || clazz == LocalDate.class
                || clazz == LocalDateTime.class || clazz == LocalTime.class;
    }

    private static boolean isStringType(final Class<?> clazz) {
        return CharSequence.class.isAssignableFrom(clazz) || clazz == char.class
                || clazz == Character.class;
    }

    private static boolean isBooleanType(final Class<?> clazz) {
        return clazz == boolean.class || clazz == Boolean.class;
    }

    private static boolean isIntegerType(final Class<?> clazz) {
        return clazz == byte.class || clazz == Byte.class
                || clazz == short.class || clazz == Short.class
                || clazz == int.class || clazz == Integer.class
                || clazz == long.class || clazz == Long.class;
    }

    private static boolean isNumberType(final Class<?> clazz) {
        return clazz == float.class || clazz == Float.class
                || clazz == double.class || clazz == Double.class;
    }

    public static class ResponseType {

        private String name;

        private String description;

        private String type;

        /**
         * child fields.
         */
        private List<ResponseType> refs;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public List<ResponseType> getRefs() {
            return refs;
        }

        public void setRefs(final List<ResponseType> refs) {
            this.refs = refs;
        }
    }


    public static class Parameter {

        private String name;

        private String in;

        private String description;

        private boolean required;

        private Schema schema;

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
         * get in.
         *
         * @return in
         */
        public String getIn() {
            return in;
        }

        /**
         * set in.
         *
         * @param in in
         */
        public void setIn(final String in) {
            this.in = in;
        }

        /**
         * get description.
         *
         * @return description
         */
        public String getDescription() {
            return description;
        }

        /**
         * set description.
         *
         * @param description description
         */
        public void setDescription(final String description) {
            this.description = description;
        }

        /**
         * get required.
         *
         * @return required
         */
        public boolean isRequired() {
            return required;
        }

        /**
         * set required.
         *
         * @param required required
         */
        public void setRequired(final boolean required) {
            this.required = required;
        }

        /**
         * get schema.
         *
         * @return schema
         */
        public Schema getSchema() {
            return schema;
        }

        /**
         * set schema.
         *
         * @param schema schema
         */
        public void setSchema(final Schema schema) {
            this.schema = schema;
        }
    }

    public static class Schema {

        private String type;

        private String format;

        public Schema(final String type, final String format) {
            this.type = type;
            this.format = format;
        }

        /**
         * get type.
         *
         * @return type
         */
        public String getType() {
            return type;
        }

        /**
         * set type.
         *
         * @param type type
         */
        public void setType(final String type) {
            this.type = type;
        }

        /**
         * get format.
         *
         * @return format
         */
        public String getFormat() {
            return format;
        }

        /**
         * set format.
         *
         * @param format format
         */
        public void setFormat(final String format) {
            this.format = format;
        }
    }

}
