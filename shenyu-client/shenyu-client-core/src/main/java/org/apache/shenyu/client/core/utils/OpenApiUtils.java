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
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
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
import java.util.Arrays;
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
     * Check if the given RPC type uses Spring MVC parameter parsing.
     * HTTP, WebSocket and Spring Cloud types use Spring MVC annotations for parameter resolution,
     * while other RPC types (Dubbo, gRPC, etc.) parse parameters from Java method signatures directly.
     *
     * @param rpcTypeEnum the RPC type enum
     * @return true if Spring MVC parameter parsing should be used
     */
    public static boolean useSpringMvcParamParsing(final RpcTypeEnum rpcTypeEnum) {
        return rpcTypeEnum == RpcTypeEnum.HTTP
                || rpcTypeEnum == RpcTypeEnum.WEB_SOCKET
                || rpcTypeEnum == RpcTypeEnum.SPRING_CLOUD;
    }

    /**
     * Build document JSON string for the given API method.
     * Dispatches to the appropriate parameter/response generation based on RPC type.
     *
     * @param tags       the API tags
     * @param path       the API path
     * @param method     the Java method
     * @param rpcTypeEnum the RPC type
     * @return document JSON string
     */
    public static String buildDocumentJson(final List<String> tags, final String path,
                                           final Method method, final RpcTypeEnum rpcTypeEnum) {
        boolean useSpringMvcParamParsing = useSpringMvcParamParsing(rpcTypeEnum);
        Map<String, Object> documentMap;
        if (useSpringMvcParamParsing) {
            documentMap = ImmutableMap.<String, Object>builder()
                    .put("tags", tags)
                    .put("operationId", path)
                    .put("requestParameters", generateRequestDocParameters(path, method))
                    .put("responseParameters", Collections.singletonList(parseReturnType(method)))
                    .put("responses", generateDocumentResponse(path))
                    .build();
        } else if (rpcTypeEnum == RpcTypeEnum.GRPC) {
            documentMap = ImmutableMap.<String, Object>builder()
                    .put("tags", tags)
                    .put("operationId", path)
                    .put("requestParameters", generateGrpcRequestDocParameters(method))
                    .put("responseParameters", Collections.singletonList(parseGrpcReturnType(method)))
                    .put("responses", generateGrpcDocumentResponse(path, method))
                    .build();
        } else {
            documentMap = ImmutableMap.<String, Object>builder()
                    .put("tags", tags)
                    .put("operationId", path)
                    .put("requestParameters", generateRpcRequestDocParameters(method))
                    .put("responseParameters", Collections.singletonList(parseReturnType(method)))
                    .put("responses", generateRpcDocumentResponse(path, method))
                    .build();
        }
        return GsonUtils.getInstance().toJson(documentMap);
    }


    /**
     * Generate request parameters for HTTP methods.
     * This produces Parameter objects with name, in, type, and refs fields.
     *
     * @param path   the api path
     * @param method the method
     * @return request parameters
     */
    public static List<Parameter> generateRequestDocParameters(final String path, final Method method) {
        List<Parameter> list = new ArrayList<>();
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
                        parameter.setType("string");
                        list.add(parameter);
                    }
                }
            }
        }
        List<String> segments = UrlPathUtils.getSegments(path);
        for (String segment : segments) {
            if (EVERY_PATH.equals(segment)) {
                Parameter parameter = new Parameter();
                parameter.setIn("path");
                parameter.setName(segment);
                parameter.setRequired(true);
                parameter.setType("string");
                list.add(parameter);
            }
            if (segment.startsWith(LEFT_ANGLE_BRACKETS) && segment.endsWith(RIGHT_ANGLE_BRACKETS)) {
                String name = segment.substring(1, segment.length() - 1);
                Parameter parameter = new Parameter();
                parameter.setIn("path");
                parameter.setName(name);
                parameter.setRequired(true);
                parameter.setType("string");
                list.add(parameter);
            }
        }
        return list;
    }

    /**
     * Generate request parameters for RPC methods.
     * Unlike HTTP methods that use Spring annotations, RPC method parameters
     * are parsed from Java method parameter types directly.

     * @param method the method
     * @return request parameters
     */
    public static List<Parameter> generateRpcRequestDocParameters(final Method method) {
        List<Parameter> list = new ArrayList<>();
        java.lang.reflect.Parameter[] methodParams = method.getParameters();
        for (java.lang.reflect.Parameter methodParam : methodParams) {
            Type paramType = methodParam.getParameterizedType();
            Schema schema = parseSchema(paramType, 0, new HashMap<>(16));
            Parameter parameter = convertSchemaToParameter(methodParam.getName(), schema);
            parameter.setRequired(true);
            list.add(parameter);
        }
        return list;
    }

    /**
     * Generate request parameters for gRPC methods.
     * gRPC method signatures differ from other RPC types:
     * - Unary/ServerStreaming: void method(Request req, StreamObserver{Response} observer)
     * - ClientStreaming/BidiStreaming: StreamObserver{Request} method(StreamObserver{Response} observer)
     * StreamObserver parameters are excluded from request parameters.
     *
     * @param method the method
     * @return request parameters
     */
    public static List<Parameter> generateGrpcRequestDocParameters(final Method method) {
        List<Parameter> list = new ArrayList<>();
        java.lang.reflect.Parameter[] methodParams = method.getParameters();
        for (java.lang.reflect.Parameter methodParam : methodParams) {
            if (isStreamObserver(methodParam.getType())) {
                continue;
            }
            Type paramType = methodParam.getParameterizedType();
            Schema schema = parseSchema(paramType, 0, new HashMap<>(16));
            Parameter parameter = convertSchemaToParameter(methodParam.getName(), schema);
            parameter.setRequired(true);
            list.add(parameter);
        }
        if (list.isEmpty()) {
            Type returnType = method.getGenericReturnType();
            Type actualType = extractStreamObserverTypeParam(returnType);
            if (Objects.nonNull(actualType)) {
                Schema schema = parseSchema(actualType, 0, new HashMap<>(16));
                Parameter parameter = convertSchemaToParameter("request", schema);
                parameter.setRequired(true);
                list.add(parameter);
            }
        }
        return list;
    }

    private static Parameter convertSchemaToParameter(final String name, final Schema schema) {
        Parameter parameter = new Parameter();
        parameter.setName(name);
        parameter.setType(schema.getType());
        if (Objects.nonNull(schema.getRefs()) && !schema.getRefs().isEmpty()) {
            List<Parameter> refs = new ArrayList<>();
            for (Schema ref : schema.getRefs()) {
                refs.add(convertSchemaToParameter(ref.getName(), ref));
            }
            parameter.setRefs(refs);
        }
        return parameter;
    }

    /**
     * Parse return type for gRPC methods.
     * - Unary/ServerStreaming: response type is extracted from StreamObserver{Response} parameter
     * - ClientStreaming/BidiStreaming: response type is extracted from StreamObserver{Response} parameter
     *
     * @param method the method
     * @return response type
     */
    public static ResponseType parseGrpcReturnType(final Method method) {
        java.lang.reflect.Parameter[] methodParams = method.getParameters();
        for (java.lang.reflect.Parameter methodParam : methodParams) {
            if (isStreamObserver(methodParam.getType())) {
                Type paramType = methodParam.getParameterizedType();
                Type actualType = extractStreamObserverTypeParam(paramType);
                if (Objects.nonNull(actualType)) {
                    return parseType("ROOT", actualType, 0, new HashMap<>(16));
                }
            }
        }
        ResponseType voidType = new ResponseType();
        voidType.setName("ROOT");
        voidType.setType("void");
        return voidType;
    }

    /**
     * Generate document response for gRPC methods.
     *
     * @param path   the api path
     * @param method the method
     * @return documentResponseMap
     */
    public static Map<String, Object> generateGrpcDocumentResponse(final String path, final Method method) {
        String returnTypeStr = "void";
        java.lang.reflect.Parameter[] methodParams = method.getParameters();
        for (java.lang.reflect.Parameter methodParam : methodParams) {
            if (isStreamObserver(methodParam.getType())) {
                Type paramType = methodParam.getParameterizedType();
                Type actualType = extractStreamObserverTypeParam(paramType);
                if (Objects.nonNull(actualType)) {
                    returnTypeStr = resolveTypeName(actualType);
                }
                break;
            }
        }
        ImmutableMap<Object, Object> contentMap = ImmutableMap.builder()
                .put(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE, ImmutableMap.of("schema", ImmutableMap.of("type", returnTypeStr)))
                .build();
        ImmutableMap<Object, Object> successMap = ImmutableMap.builder()
                .put("description", path)
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> notFoundMap = ImmutableMap.builder()
                .put("description", StringUtils.join("the path [", path, "] not found"))
                .put("content", ImmutableMap.of(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE,
                        ImmutableMap.of("schema", ImmutableMap.of("type", "string")))).build();
        return ImmutableMap.<String, Object>builder()
                .put("200", successMap)
                .put("404", notFoundMap)
                .build();
    }

    private static boolean isStreamObserver(final Class<?> clazz) {
        return "io.grpc.stub.StreamObserver".equals(clazz.getName());
    }

    private static Type extractStreamObserverTypeParam(final Type type) {
        if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            if ("io.grpc.stub.StreamObserver".equals(((Class<?>) pt.getRawType()).getName())) {
                Type[] actualTypes = pt.getActualTypeArguments();
                if (actualTypes.length > 0) {
                    return actualTypes[0];
                }
            }
        }
        return null;
    }

    private static Pair<Boolean, Annotation[][]> isQuery(final Method method) {
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        for (Annotation[] parameterAnnotation : parameterAnnotations) {
            if (parameterAnnotation.length > 0 && isQueryName(parameterAnnotation[0].annotationType().getName(), QUERY_CLASSES)) {
                return Pair.of(true, parameterAnnotations);
            }
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
     * Generate document response for RPC methods with actual return type info.
     *
     * @param path   the api path
     * @param method the method
     * @return documentResponseMap
     */
    public static Map<String, Object> generateRpcDocumentResponse(final String path, final Method method) {
        Type returnType = method.getGenericReturnType();
        String returnTypeStr = "void".equals(returnType.getTypeName()) ? "void" : resolveTypeName(returnType);
        ImmutableMap<Object, Object> contentMap = ImmutableMap.builder()
                .put(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE, ImmutableMap.of("schema", ImmutableMap.of("type", returnTypeStr)))
                .build();
        ImmutableMap<Object, Object> successMap = ImmutableMap.builder()
                .put("description", path)
                .put("content", contentMap).build();
        ImmutableMap<Object, Object> notFoundMap = ImmutableMap.builder()
                .put("description", StringUtils.join("the path [", path, "] not found"))
                .put("content", ImmutableMap.of(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE,
                        ImmutableMap.of("schema", ImmutableMap.of("type", "string")))).build();
        return ImmutableMap.<String, Object>builder()
                .put("200", successMap)
                .put("404", notFoundMap)
                .build();
    }

    private static String resolveTypeName(final Type type) {
        if (type instanceof Class) {
            return resolveTypeName((Class<?>) type);
        }
        if (type instanceof ParameterizedType) {
            Class<?> rawType = (Class<?>) ((ParameterizedType) type).getRawType();
            if (Collection.class.isAssignableFrom(rawType)) {
                return "array";
            }
            return "object";
        }
        return "object";
    }

    private static String resolveTypeName(final Class<?> clazz) {
        if (isBooleanType(clazz)) {
            return "boolean";
        } else if (isIntegerType(clazz)) {
            return "integer";
        } else if (isNumberType(clazz)) {
            return "number";
        } else if (isStringType(clazz)) {
            return "string";
        } else if (isDateType(clazz)) {
            return "string";
        } else if (clazz.isArray() || Collection.class.isAssignableFrom(clazz)) {
            return "array";
        } else if (clazz.isEnum()) {
            return "string";
        } else if (isProtobufMessage(clazz)) {
            return "object";
        } else if (Map.class.isAssignableFrom(clazz)) {
            return "object";
        } else {
            return "object";
        }
    }

    /**
     * Check if the class is a protobuf-generated message class.
     * Uses reflection to avoid direct protobuf dependency.
     *
     * @param clazz the class to check
     * @return true if it's a protobuf message class
     */
    static boolean isProtobufMessage(final Class<?> clazz) {
        if (Objects.isNull(clazz)) {
            return false;
        }
        Class<?> current = clazz;
        while (Objects.nonNull(current) && current != Object.class) {
            String className = current.getName();
            if ("com.google.protobuf.GeneratedMessageV3".equals(className)
                    || "com.google.protobuf.GeneratedMessage".equals(className)
                    || "com.google.protobuf.GeneratedMessageLite".equals(className)) {
                return true;
            }
            current = current.getSuperclass();
        }
        return false;
    }

    /**
     * Check if the class is com.google.protobuf.Empty.
     *
     * @param clazz the class to check
     * @return true if it's protobuf Empty
     */
    private static boolean isProtobufEmpty(final Class<?> clazz) {
        return Objects.nonNull(clazz) && "com.google.protobuf.Empty".equals(clazz.getName());
    }

    /**
     * Parse protobuf message fields using reflection on getDescriptor().
     * Protobuf descriptor types are mapped to OpenAPI types.
     *
     * @param responseType    the response type to populate
     * @param clazz           the protobuf message class
     * @param depth           current recursion depth
     * @param typeVariableMap type variable map
     * @return populated ResponseType
     */
    private static ResponseType parseProtobufClass(final ResponseType responseType, final Class<?> clazz,
                                                   final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        if (isProtobufEmpty(clazz)) {
            responseType.setType("object");
            return responseType;
        }
        try {
            java.lang.reflect.Method getDescriptorMethod = clazz.getMethod("getDescriptor");
            Object descriptor = getDescriptorMethod.invoke(null);
            java.lang.reflect.Method getFieldsMethod = descriptor.getClass().getMethod("getFields");
            @SuppressWarnings("unchecked")
            List<?> fields = (List<?>) getFieldsMethod.invoke(descriptor);
            List<ResponseType> refs = parseProtobufFields(fields, clazz, depth, typeVariableMap);
            responseType.setType("object");
            responseType.setRefs(refs);
        } catch (Exception e) {
            responseType.setType("object");
        }
        return responseType;
    }

    private static List<ResponseType> parseProtobufFields(final List<?> fields, final Class<?> clazz,
                                                          final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        List<ResponseType> refs = new ArrayList<>();
        java.lang.reflect.Method getNameMethod = null;
        java.lang.reflect.Method getTypeMethod = null;
        java.lang.reflect.Method isRepeatedMethod = null;
        java.lang.reflect.Method getMessageTypeMethod = null;
        for (Object field : fields) {
            if (Objects.isNull(getNameMethod)) {
                getNameMethod = field.getClass().getMethod("getName");
                getTypeMethod = field.getClass().getMethod("getType");
                isRepeatedMethod = field.getClass().getMethod("isRepeated");
                getMessageTypeMethod = field.getClass().getMethod("getMessageType");
            }
            String fieldName = (String) getNameMethod.invoke(field);
            Object fieldType = getTypeMethod.invoke(field);
            boolean isRepeated = (boolean) isRepeatedMethod.invoke(field);
            refs.add(parseProtobufField(fieldName, fieldType, isRepeated,
                    getMessageTypeMethod, field, clazz, depth, typeVariableMap));
        }
        return refs;
    }

    private static ResponseType parseProtobufField(final String fieldName, final Object fieldType, final boolean isRepeated,
                                                   final java.lang.reflect.Method getMessageTypeMethod, final Object field,
                                                   final Class<?> clazz, final int depth,
                                                   final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        if (isRepeated) {
            return parseRepeatedProtobufField(fieldName, fieldType, getMessageTypeMethod, field);
        }
        String fieldTypeName = fieldType.toString();
        ResponseType fieldResponse = new ResponseType();
        fieldResponse.setName(fieldName);
        if ("MESSAGE".equals(fieldTypeName)) {
            resolveProtobufMessageField(fieldResponse, getMessageTypeMethod, field, clazz, depth, typeVariableMap);
        } else if ("ENUM".equals(fieldTypeName)) {
            fieldResponse.setType("string");
        } else {
            fieldResponse.setType(mapProtobufTypeToOpenApi(fieldTypeName));
        }
        return fieldResponse;
    }

    private static ResponseType parseRepeatedProtobufField(final String fieldName, final Object fieldType,
                                                           final java.lang.reflect.Method getMessageTypeMethod,
                                                           final Object field) throws Exception {
        ResponseType arrayType = new ResponseType();
        arrayType.setName(fieldName);
        arrayType.setType("array");
        String fieldTypeName = fieldType.toString();
        ResponseType elementType = new ResponseType();
        elementType.setName("ITEMS");
        if ("MESSAGE".equals(fieldTypeName)) {
            Object msgDescriptor = getMessageTypeMethod.invoke(field);
            java.lang.reflect.Method getFullNameMethod = msgDescriptor.getClass().getMethod("getFullName");
            String fullMsgName = (String) getFullNameMethod.invoke(msgDescriptor);
            elementType.setType("object");
            elementType.setDescription(fullMsgName);
        } else {
            elementType.setType(mapProtobufTypeToOpenApi(fieldTypeName));
        }
        arrayType.setRefs(Collections.singletonList(elementType));
        return arrayType;
    }

    private static void resolveProtobufMessageField(final ResponseType fieldResponse,
                                                    final java.lang.reflect.Method getMessageTypeMethod,
                                                    final Object field, final Class<?> clazz,
                                                    final int depth,
                                                    final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        Object msgDescriptor = getMessageTypeMethod.invoke(field);
        java.lang.reflect.Method getFullNameMethod = msgDescriptor.getClass().getMethod("getFullName");
        String fullMsgName = (String) getFullNameMethod.invoke(msgDescriptor);
        fieldResponse.setType("object");
        fieldResponse.setDescription(fullMsgName);
        if (depth < 5) {
            try {
                Class<?> nestedClass = Class.forName(toJavaClassName(clazz, fullMsgName));
                List<ResponseType> nestedRefs = parseProtobufClass(new ResponseType(), nestedClass, depth + 1, typeVariableMap).getRefs();
                if (Objects.nonNull(nestedRefs) && !nestedRefs.isEmpty()) {
                    fieldResponse.setRefs(nestedRefs);
                }
            } catch (ClassNotFoundException ignored) {
            }
        }
    }

    private static String mapProtobufTypeToOpenApi(final String protobufType) {
        switch (protobufType) {
            case "INT32":
            case "INT64":
            case "UINT32":
            case "UINT64":
            case "SINT32":
            case "SINT64":
            case "FIXED32":
            case "FIXED64":
            case "SFIXED32":
            case "SFIXED64":
                return "integer";
            case "FLOAT":
            case "DOUBLE":
                return "number";
            case "BOOL":
                return "boolean";
            case "STRING":
            case "BYTES":
                return "string";
            default:
                return "string";
        }
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
        } else if (isProtobufMessage(clazz)) {
            return parseProtobufClass(responseType, clazz, depth, typeVariableMap);
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

    private static Schema parseSchema(final Type type, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        if (depth > 5) {
            return new Schema("object", null);
        }
        if (type instanceof Class) {
            return parseClassSchema((Class<?>) type, depth, typeVariableMap);
        } else if (type instanceof ParameterizedType) {
            return parseParameterizedTypeSchema((ParameterizedType) type, depth, typeVariableMap);
        } else if (type instanceof GenericArrayType) {
            Schema elementSchema = parseSchema(((GenericArrayType) type).getGenericComponentType(), depth + 1, typeVariableMap);
            Schema schema = new Schema("array", null);
            schema.setRefs(Collections.singletonList(elementSchema));
            return schema;
        } else if (type instanceof TypeVariable) {
            Type actualType = typeVariableMap.get(type);
            if (Objects.nonNull(actualType)) {
                return parseSchema(actualType, depth, typeVariableMap);
            } else if (((TypeVariable<?>) type).getBounds().length > 0) {
                return parseSchema(((TypeVariable<?>) type).getBounds()[0], depth, typeVariableMap);
            } else {
                return new Schema("object", null);
            }
        } else {
            return new Schema("object", null);
        }
    }

    private static Schema parseClassSchema(final Class<?> clazz, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        if (clazz.isArray()) {
            Schema elementSchema = parseSchema(clazz.getComponentType(), depth + 1, typeVariableMap);
            Schema schema = new Schema("array", null);
            schema.setRefs(Collections.singletonList(elementSchema));
            return schema;
        } else if (clazz.isEnum()) {
            return new Schema("string", null);
        } else if (isBooleanType(clazz)) {
            return new Schema("boolean", null);
        } else if (isIntegerType(clazz)) {
            return new Schema("integer", null);
        } else if (isNumberType(clazz)) {
            return new Schema("number", null);
        } else if (isStringType(clazz)) {
            return new Schema("string", null);
        } else if (isDateType(clazz)) {
            return new Schema("string", "date");
        } else if (Collection.class.isAssignableFrom(clazz)) {
            return new Schema("array", null);
        } else if (Map.class.isAssignableFrom(clazz)) {
            return new Schema("object", null);
        } else if (isProtobufMessage(clazz)) {
            return parseProtobufClassSchema(clazz, depth, typeVariableMap);
        } else {
            List<Schema> refs = new ArrayList<>();
            for (Field field : clazz.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }
                Schema fieldSchema = parseSchema(field.getGenericType(), depth + 1, typeVariableMap);
                fieldSchema.setName(field.getName());
                refs.add(fieldSchema);
            }
            Schema schema = new Schema("object", null);
            schema.setRefs(refs);
            return schema;
        }
    }

    private static Schema parseParameterizedTypeSchema(final ParameterizedType type, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        Class<?> rawType = (Class<?>) type.getRawType();
        Type[] actualTypeArguments = type.getActualTypeArguments();
        TypeVariable<?>[] typeVariables = rawType.getTypeParameters();
        Map<TypeVariable<?>, Type> newTypeVariableMap = new HashMap<>(typeVariableMap);
        for (int i = 0; i < typeVariables.length; i++) {
            newTypeVariableMap.put(typeVariables[i], actualTypeArguments[i]);
        }
        if (Collection.class.isAssignableFrom(rawType)) {
            Schema elementSchema = parseSchema(actualTypeArguments[0], depth + 1, newTypeVariableMap);
            elementSchema.setName("items");
            Schema schema = new Schema("array", null);
            schema.setRefs(Collections.singletonList(elementSchema));
            return schema;
        } else if (Map.class.isAssignableFrom(rawType)) {
            Schema keySchema = parseSchema(actualTypeArguments[0], depth + 1, newTypeVariableMap);
            keySchema.setName("key");
            Schema valueSchema = parseSchema(actualTypeArguments[1], depth + 1, newTypeVariableMap);
            valueSchema.setName("value");
            Schema schema = new Schema("object", null);
            schema.setRefs(Arrays.asList(keySchema, valueSchema));
            return schema;
        } else {
            List<Schema> refs = new ArrayList<>();
            for (Field field : rawType.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }
                Schema fieldSchema = parseSchema(field.getGenericType(), depth + 1, newTypeVariableMap);
                fieldSchema.setName(field.getName());
                refs.add(fieldSchema);
            }
            Schema schema = new Schema("object", null);
            schema.setRefs(refs);
            return schema;
        }
    }

    private static Schema parseProtobufClassSchema(final Class<?> clazz, final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) {
        if (isProtobufEmpty(clazz)) {
            return new Schema("object", null);
        }
        try {
            java.lang.reflect.Method getDescriptorMethod = clazz.getMethod("getDescriptor");
            Object descriptor = getDescriptorMethod.invoke(null);
            java.lang.reflect.Method getFieldsMethod = descriptor.getClass().getMethod("getFields");
            @SuppressWarnings("unchecked")
            List<?> fields = (List<?>) getFieldsMethod.invoke(descriptor);
            List<Schema> refs = parseProtobufFieldsSchema(fields, clazz, depth, typeVariableMap);
            Schema schema = new Schema("object", null);
            schema.setRefs(refs);
            return schema;
        } catch (Exception e) {
            return new Schema("object", null);
        }
    }

    private static List<Schema> parseProtobufFieldsSchema(final List<?> fields, final Class<?> clazz,
                                                          final int depth, final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        List<Schema> refs = new ArrayList<>();
        java.lang.reflect.Method getNameMethod = null;
        java.lang.reflect.Method getTypeMethod = null;
        java.lang.reflect.Method isRepeatedMethod = null;
        java.lang.reflect.Method getMessageTypeMethod = null;
        for (Object field : fields) {
            if (Objects.isNull(getNameMethod)) {
                getNameMethod = field.getClass().getMethod("getName");
                getTypeMethod = field.getClass().getMethod("getType");
                isRepeatedMethod = field.getClass().getMethod("isRepeated");
                getMessageTypeMethod = field.getClass().getMethod("getMessageType");
            }
            String fieldName = (String) getNameMethod.invoke(field);
            Object fieldType = getTypeMethod.invoke(field);
            boolean isRepeated = (boolean) isRepeatedMethod.invoke(field);
            refs.add(parseProtobufFieldSchema(fieldName, fieldType, isRepeated,
                    getMessageTypeMethod, field, clazz, depth, typeVariableMap));
        }
        return refs;
    }

    private static Schema parseProtobufFieldSchema(final String fieldName, final Object fieldType, final boolean isRepeated,
                                                   final java.lang.reflect.Method getMessageTypeMethod, final Object field,
                                                   final Class<?> clazz, final int depth,
                                                   final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        if (isRepeated) {
            return parseRepeatedProtobufFieldSchema(fieldName, fieldType, getMessageTypeMethod, field, clazz, depth, typeVariableMap);
        }
        String fieldTypeName = fieldType.toString();
        Schema schema;
        if ("MESSAGE".equals(fieldTypeName)) {
            schema = resolveProtobufMessageFieldSchema(getMessageTypeMethod, field, clazz, depth, typeVariableMap);
        } else if ("ENUM".equals(fieldTypeName)) {
            schema = new Schema("string", null);
        } else {
            schema = new Schema(mapProtobufTypeToOpenApi(fieldTypeName), null);
        }
        schema.setName(fieldName);
        return schema;
    }

    private static Schema parseRepeatedProtobufFieldSchema(final String fieldName, final Object fieldType,
                                                           final java.lang.reflect.Method getMessageTypeMethod,
                                                           final Object field,
                                                           final Class<?> clazz, final int depth,
                                                           final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        String fieldTypeName = fieldType.toString();
        Schema elementSchema;
        if ("MESSAGE".equals(fieldTypeName)) {
            elementSchema = resolveProtobufMessageFieldSchema(getMessageTypeMethod, field, clazz, depth, typeVariableMap);
        } else if ("ENUM".equals(fieldTypeName)) {
            elementSchema = new Schema("string", null);
        } else {
            elementSchema = new Schema(mapProtobufTypeToOpenApi(fieldTypeName), null);
        }
        elementSchema.setName("items");
        Schema schema = new Schema("array", null);
        schema.setName(fieldName);
        schema.setRefs(Collections.singletonList(elementSchema));
        return schema;
    }

    private static Schema resolveProtobufMessageFieldSchema(final java.lang.reflect.Method getMessageTypeMethod,
                                                            final Object field, final Class<?> clazz,
                                                            final int depth,
                                                            final Map<TypeVariable<?>, Type> typeVariableMap) throws Exception {
        Object msgDescriptor = getMessageTypeMethod.invoke(field);
        java.lang.reflect.Method getFullNameMethod = msgDescriptor.getClass().getMethod("getFullName");
        String fullMsgName = (String) getFullNameMethod.invoke(msgDescriptor);
        Schema schema = new Schema("object", null);
        if (depth < 5) {
            try {
                Class<?> nestedClass = Class.forName(toJavaClassName(clazz, fullMsgName));
                List<Schema> nestedRefs = parseProtobufClassSchema(nestedClass, depth + 1, typeVariableMap).getRefs();
                if (Objects.nonNull(nestedRefs) && !nestedRefs.isEmpty()) {
                    schema.setRefs(nestedRefs);
                }
            } catch (ClassNotFoundException ignored) {
            }
        }
        return schema;
    }

    private static String toJavaClassName(final Class<?> contextClass, final String protobufFullName) {
        String packageName = contextClass.getPackage().getName();
        if (protobufFullName.startsWith(packageName + ".")) {
            String relativeName = protobufFullName.substring(packageName.length() + 1).replace('.', '$');
            Class<?> declaringClass = contextClass.getDeclaringClass();
            if (Objects.nonNull(declaringClass)) {
                return declaringClass.getName() + "$" + relativeName;
            }
            return packageName + "." + relativeName;
        }
        return protobufFullName.replace('.', '$');
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

        private String type;

        private List<Parameter> refs;

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getIn() {
            return in;
        }

        public void setIn(final String in) {
            this.in = in;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(final String description) {
            this.description = description;
        }

        public boolean isRequired() {
            return required;
        }

        public void setRequired(final boolean required) {
            this.required = required;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public List<Parameter> getRefs() {
            return refs;
        }

        public void setRefs(final List<Parameter> refs) {
            this.refs = refs;
        }
    }

    public static class Schema {

        private String name;

        private String type;

        private String format;

        private List<Schema> refs;

        public Schema(final String type, final String format) {
            this.type = type;
            this.format = format;
        }

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public String getType() {
            return type;
        }

        public void setType(final String type) {
            this.type = type;
        }

        public String getFormat() {
            return format;
        }

        public void setFormat(final String format) {
            this.format = format;
        }

        public List<Schema> getRefs() {
            return refs;
        }

        public void setRefs(final List<Schema> refs) {
            this.refs = refs;
        }
    }

}
