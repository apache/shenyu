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

import org.dromara.soul.common.exception.SoulException;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Binder .
 * <p>
 * <p>
 * 2019-08-13 20:53
 *
 * @author chenbin sixh
 */
public final class Binder {
    public final <T> Object bind(String sf, BindData<T> target) {
        return bind(PropertyName.of(sf), target);
    }

    public final <T> Object bind(PropertyName name, BindData<T> target) {
//        handler = (handler != null ? handler : BindHandler.DEFAULT);
//        Context context = new Context();
        Object bind = bindObject(name, target, false);
//        return BindResult.of(bound);
        return null;
    }

    private <T> Object bindObject(PropertyName name, BindData<T> target,
                                  boolean allowRecursiveBinding) {
        ConfigProperty property = findProperty(name, context);
        if (property == null) {
            return null;
        }
        AggregateBinder<?> aggregateBinder = getAggregateBinder(target);
        if (aggregateBinder != null) {
            return bindAggregate(name, target, aggregateBinder);
        }
        if (property != null) {
            try {
                return bindProperty(target, property);
            } catch (SoulException ex) {
                // We might still be able to bind it as a bean
                Object bean = bindBean(name, target,
                        allowRecursiveBinding);
                if (bean != null) {
                    return bean;
                }
                throw ex;
            }
        }
        return bindBean(name, target, allowRecursiveBinding);
    }

    private <T> Object bindProperty(BindData<T> target, ConfigProperty property) {
        return null;
    }

    private <T> Object bindBean(PropertyName name, BindData<T> target, boolean allowRecursiveBinding) {
//        if (containsNoDescendantOf(context.streamSources(), name)
//                || isUnbindableBean(name, target, context)) {
//            return null;
//        }
//        BeanPropertyBinder propertyBinder = (propertyName, propertyTarget) -> bind(
//                name.append(propertyName), propertyTarget, handler, context, false);
//        Class<?> type = target.getType().resolve(Object.class);
//        if (!allowRecursiveBinding && context.hasBoundBean(type)) {
//            return null;
//        }
//        return context.withBean(type, () -> {
//            Stream<?> boundBeans = BEAN_BINDERS.stream()
//                    .map((b) -> b.bind(name, target, context, propertyBinder));
//            return boundBeans.filter(Objects::nonNull).findFirst().orElse(null);
//        });
        return null;
    }

    private <T> Object bindAggregate(PropertyName name, BindData<T> target,
                                     AggregateBinder<?> aggregateBinder) {
        return null;
    }

    private AggregateBinder<?> getAggregateBinder(BindData<?> target) {
        Type type = target.getType();
        if (Map.class.isAssignableFrom(type.getClass())) {
            return new MapBinder();
        }
        if (Collection.class.isAssignableFrom(type.getClass())) {
            return new CollectionBinder();
        }
        if (isArray(type)) {
            return new ArrayBinder();
        }
        return null;
    }

    private boolean isArray(Type type) {
        return ((type instanceof Class && ((Class<?>) type).isArray());
    }
}
