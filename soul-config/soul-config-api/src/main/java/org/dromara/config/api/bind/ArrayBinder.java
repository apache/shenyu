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

import org.dromara.config.api.source.PropertyName;

import java.lang.reflect.Array;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

/**
 * ArrayBinder .
 * <p>
 * <p>
 * 2019-08-13 21:10
 *
 * @author chenbin sixh
 */
public class ArrayBinder extends IndexedBinder<Object> {

    public ArrayBinder(Binder.Env env) {
        super(env);
    }


    @Override
    Object bindAggregate(PropertyName name, BindData<?> target, AggregateElementBinder elementBinder) {
        IndexedCollectionSupplier result = new IndexedCollectionSupplier(ArrayList::new);
        Type aggregateType = target.getType();
        bindIndexed(name, target, elementBinder, aggregateType, Object.class, result);
        if (result.wasSupplied()) {
            List<Object> list = (List<Object>) result.get();
            Object array = Array.newInstance(target.getComponentType(), list.size());
            for (int i = 0; i < list.size(); i++) {
                Array.set(array, i, list.get(i));
            }
            return array;
        }
        return null;
    }

    @Override
    Object assemble(Supplier<?> inst, Object additional) {
        return additional;
    }
}
