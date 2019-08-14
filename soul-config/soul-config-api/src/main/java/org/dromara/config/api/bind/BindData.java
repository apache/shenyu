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

import com.fasterxml.jackson.databind.introspect.AnnotatedMember;
import lombok.Data;

import java.lang.reflect.Type;
import java.util.function.Supplier;

/**
 * BindData .
 * <p>
 * <p>
 * 2019-08-13 20:57
 *
 * @author chenbin sixh
 */
@Data
public class BindData<T> {

    private Type type;

    private Type boxedType;

    private Supplier<T> inst;

    public BindData(Type type, Type boxedType, Supplier<T> inst) {
        this.type = type;
        this.boxedType = boxedType;
        this.inst = inst;
    }

    public static <T> BindData<T> of(Type type) {

        return new BindData<>(type, type, null);
    }

    public <T> BindData<T> withSuppliedValue(Supplier<T> value) {
        return new BindData<>(this.type, this.type, value);
    }
}
