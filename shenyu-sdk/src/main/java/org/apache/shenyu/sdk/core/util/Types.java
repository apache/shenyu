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

package org.apache.shenyu.sdk.core.util;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

/**
 * Types.
 */
public final class Types {

    private Types() {
        // No instances.
    }

    /**
     * resolveReturnType.
     *
     * @param baseType baseType
     * @param overridingType overridingType
     * @return {@link Type}
     */
    public static Type resolveReturnType(final Type baseType, final Type overridingType) {
        if (baseType instanceof Class && overridingType instanceof Class
                && ((Class<?>) baseType).isAssignableFrom((Class<?>) overridingType)) {
            // NOTE: javac generates multiple same methods for multiple inherited generic interfaces
            return overridingType;
        }
        if (baseType instanceof Class && overridingType instanceof ParameterizedType) {
            // NOTE: javac will generate multiple methods with different return types
            // base interface declares generic method, override declares parameterized generic method
            return overridingType;
        }
        if (baseType instanceof Class && overridingType instanceof TypeVariable) {
            // NOTE: javac will generate multiple methods with different return types
            // base interface declares non generic method, override declares generic method
            return overridingType;
        }
        return baseType;
    }
}
