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

package org.apache.shenyu.plugin.motan.util;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * Proxy info util.
 */
public class PrxInfoUtil {

    private static final Map<String, PrimitiveType> PRIMITIVE_TYPE;

    static {
        PRIMITIVE_TYPE = new HashMap<>();
        PRIMITIVE_TYPE.put("int", new PrimitiveType(int.class, o -> {
            if (o instanceof String) {
                return Integer.valueOf((String) o);
            }
            return ((Long) o).intValue();
        }));
        PRIMITIVE_TYPE.put("double", new PrimitiveType(double.class, o -> {
            if (o instanceof String) {
                return Double.valueOf((String) o);
            }
            return o;
        }));
        PRIMITIVE_TYPE.put("long", new PrimitiveType(long.class, o -> {
            if (o instanceof String) {
                return Long.valueOf((String) o);
            }
            return o;
        }));
        PRIMITIVE_TYPE.put("short", new PrimitiveType(short.class, o -> {
            if (o instanceof String) {
                return Short.valueOf((String) o);
            }
            return ((Long) o).shortValue();
        }));
        PRIMITIVE_TYPE.put("byte", new PrimitiveType(byte.class, o -> {
            if (o instanceof String) {
                return Byte.valueOf((String) o);
            }
            return ((Long) o).byteValue();
        }));
        PRIMITIVE_TYPE.put("boolean", new PrimitiveType(boolean.class, o -> {
            if (o instanceof String) {
                return Byte.valueOf((String) o);
            }
            return o;
        }));
        PRIMITIVE_TYPE.put("char", new PrimitiveType(char.class, o -> {
            if (o instanceof String) {
                return String.valueOf(o).charAt(0);
            }
            return o;
        }));
        PRIMITIVE_TYPE.put("float", new PrimitiveType(float.class, o -> {
            if (o instanceof String) {
                return Float.valueOf((String) o);
            }
            return ((Double) o).floatValue();
        }));
    }

    /**
     * Get class type by name.
     *
     * @param className className
     * @return the type to invoke
     * @throws ClassNotFoundException ClassNotFoundException
     */
    public static Class<?> getParamClass(final String className) throws ClassNotFoundException {
        if (PRIMITIVE_TYPE.containsKey(className)) {
            return PRIMITIVE_TYPE.get(className).getClazz();
        } else {
            return Class.forName(className);
        }
    }

    static final class PrimitiveType {
        
        private final Class<?> clazz;

        private final Function<Object, Object> func;

        private PrimitiveType(final Class<?> clazz, final Function<Object, Object> func) {
            this.clazz = clazz;
            this.func = func;
        }

        public Class<?> getClazz() {
            return clazz;
        }

        public Function<Object, Object> getFunc() {
            return func;
        }
    }
}
