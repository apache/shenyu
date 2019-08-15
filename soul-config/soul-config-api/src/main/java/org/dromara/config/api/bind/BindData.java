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

package org.dromara.config.api.bind;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.function.Supplier;

/**
 * BindData .
 * <p>
 * <p>
 * 2019-08-13 20:57
 *
 * @param <T> the type parameter
 * @author chenbin sixh
 */
public class BindData<T> {

    private Type type;

    private Supplier<T> inst;

    /**
     * 字段.
     */
    private Field field;

    /**
     * Instantiates a new Bind data.
     *
     * @param type the type
     * @param inst the inst
     */
    public BindData(Type type,
                    Supplier<T> inst) {
        this(type, null, inst);
    }


    /**
     * Instantiates a new Bind data.
     *
     * @param type  the type
     * @param field the field
     * @param inst  the inst
     */
    public BindData(Type type,
                    Field field,
                    Supplier<T> inst) {
        this.type = type;
        this.inst = inst;
        this.field = field;
    }

    /**
     * Gets type.
     *
     * @return the type
     */
    public Type getType() {
        return type;
    }

    /**
     * Gets inst.
     *
     * @return the inst
     */
    public Supplier<T> getInst() {
        return inst;
    }

    /**
     * Of bind data.
     *
     * @param <T>  the type parameter
     * @param type the type
     * @return the bind data
     */
    public static <T> BindData<T> of(Type type) {

        return new BindData<>(type, null);
    }

    /**
     * Gets type class.
     *
     * @return the type class
     */
    public Class getTypeClass() {
        return (Class) type;
    }

    /**
     * Get generics type [ ].
     *
     * @return the type [ ]
     */
    public Type[] getGenerics() {
        if (field == null) {
            return new Type[0];
        }
        Type genericType = field.getGenericType();
        if (genericType instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) genericType;
            return pt.getActualTypeArguments();
        }
        return new Type[0];
    }

    /**
     * Gets component type.
     *
     * @return the component type
     */
    public Class getComponentType() {
        return this.getTypeClass().getComponentType();
    }

    /**
     * With supplied value bind data.
     *
     * @param <T>   the type parameter
     * @param field the field
     * @return the bind data
     */
    public <T> BindData<T> withField(Field field) {
        return new BindData<>(this.type, field, null);
    }

    /**
     * With supplied value bind data.
     *
     * @param <T>   the type parameter
     * @param value the value
     * @return the bind data
     */
    public <T> BindData<T> withSuppliedValue(Supplier<T> value) {
        return new BindData<>(this.type, value);
    }
}
