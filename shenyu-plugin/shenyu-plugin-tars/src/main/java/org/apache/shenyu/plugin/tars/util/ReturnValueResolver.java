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

package org.apache.shenyu.plugin.tars.util;

import com.google.common.reflect.TypeParameter;
import com.google.common.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * The tars return value type resolver.
 */
public final class ReturnValueResolver {
    
    @SuppressWarnings("rawtypes")
    private static final Map<Class, Class> WRAPPER_TYPE_MAP;
    
    static {
        WRAPPER_TYPE_MAP = new HashMap<>();
        WRAPPER_TYPE_MAP.put(int.class, Integer.class);
        WRAPPER_TYPE_MAP.put(byte.class, Byte.class);
        WRAPPER_TYPE_MAP.put(char.class, Character.class);
        WRAPPER_TYPE_MAP.put(boolean.class, Boolean.class);
        WRAPPER_TYPE_MAP.put(double.class, Double.class);
        WRAPPER_TYPE_MAP.put(float.class, Float.class);
        WRAPPER_TYPE_MAP.put(long.class, Long.class);
        WRAPPER_TYPE_MAP.put(short.class, Short.class);
    }
    
    private ReturnValueResolver() {
    }
    
    /**
     * Get return type.
     *
     * @param <T>   T
     * @param clazz clazz
     * @return the type
     */
    @SuppressWarnings("all")
    public static <T> Type getCallBackType(final Class<T> clazz) {
        return new TypeToken<CompletableFuture<T>>() { }
                .where(new TypeParameter<T>() { }, TypeToken.of(WRAPPER_TYPE_MAP.getOrDefault(clazz, clazz)))
                .getType();
    }
    
}
