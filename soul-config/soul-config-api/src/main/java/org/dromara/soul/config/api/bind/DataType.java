/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.api.bind;

import lombok.Data;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

/**
 * DataType .
 * 绑定数据字段的类型.
 * 2019-08-15
 *
 * @author sixh
 */
@Data
public class DataType {

    private Type type;

    private DataType[] generics = new DataType[0];

    private Class<?> typeClass;

    /**
     * Is assignable from boolean.
     *
     * @param clazz the clazz
     * @return the boolean
     */
    private boolean isAssignableFrom(Class<?> clazz) {
        return clazz.isAssignableFrom(typeClass);
    }

    /**
     * Is map boolean.
     *
     * @return the boolean
     */
    boolean isMap() {
        return isAssignableFrom(Map.class);
    }

    /**
     * Is collection boolean.
     *
     * @return the boolean
     */
    boolean isCollection() {
        return isAssignableFrom(Collection.class);
    }

    /**
     * Is array boolean.
     *
     * @return the boolean
     */
    boolean isArray() {
        return getTypeClass().isArray();
    }

    /**
     * 设置泛型类型.
     *
     * @param field the field
     * @return data type
     */
    DataType withGenerics(Field field) {
        if (field != null) {
            Type genericType = field.getGenericType();
            if (genericType instanceof ParameterizedType) {
                ParameterizedType pt = (ParameterizedType) genericType;
                Type[] actualTypeArguments = pt.getActualTypeArguments();
                generics = Arrays.stream(actualTypeArguments).map(DataType::of).toArray(DataType[]::new);
            }
        }
        return this;
    }

    /**
     * Instantiates a new Data type.
     *
     * @param type the type
     */
    private DataType(Type type) {
        this.type = type;
        this.typeClass = (Class<?>) type;
    }

    /**
     * Gets component type.
     *
     * @return the component type
     */
    Class<?> getComponentType() {
        return getTypeClass().getComponentType();
    }

    /**
     * Of data type.
     *
     * @param type the type
     * @return the data type
     */
    public static DataType of(Type type) {
        return new DataType(type);
    }
}
