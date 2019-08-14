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

import org.apache.commons.lang3.ClassUtils;
import org.dromara.config.api.source.ConfigProperty;
import org.dromara.config.api.source.ConfigPropertySource;
import org.dromara.config.api.source.PropertyName;
import org.dromara.soul.common.exception.SoulException;

import java.lang.reflect.Type;
import java.util.*;
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

    private List<ConfigPropertySource> source;

    private JavaBeanBinder beanBinder = new JavaBeanBinder();

    public Binder(List<ConfigPropertySource> source) {
        this.source = source;
    }

    public final <T> Object bind(String sf, BindData<T> target) {
        return bind(PropertyName.of(sf), target);
    }

    public final <T> Object bind(PropertyName name, BindData<T> target) {
        Evn env = new Evn();
        Object bind = bind(name, target, env, false);
        return bind;
    }

    protected final <T> T bind(PropertyName name, BindData<T> target,
                               Evn context, boolean allowRecursiveBinding) {
        Object bound = bindObject(name, target, allowRecursiveBinding, context);
        return (T) bound;

    }


    private <T> Object bindObject(PropertyName name, BindData<T> target,
                                  boolean allowRecursiveBinding, Evn env) {
        ConfigProperty property = findProperty(name, env);
        if (property == null && containsNoDescendantOf(env.stream(), name)) {
            return null;
        }
        AggregateBinder<?> aggregateBinder = getAggregateBinder(target, env);
        if (aggregateBinder != null) {
            return bindAggregate(name, target, aggregateBinder);
        }
        if (property != null) {
            try {
                return bindProperty(target, property, env);
            } catch (SoulException ex) {
                // We might still be able to bind it as a bean
                Object bean = bindBean(name, target,
                        allowRecursiveBinding, env);
                if (bean != null) {
                    return bean;
                }
                throw ex;
            }
        }
        return bindBean(name, target, allowRecursiveBinding, env);
    }


    private <T> Object bindProperty(BindData<T> target, ConfigProperty property, Evn evn) {
        evn.setProperty(property);
        return property.getValue();
    }

    private ConfigProperty findProperty(PropertyName name,
                                        Evn env) {
        if (name.isEmpty()) {
            return null;
        }
        return env.stream()
                .map((source) -> source.findProperty(name))
                .filter(Objects::nonNull).findFirst().orElse(null);
    }

    private <T> Object bindBean(PropertyName name, BindData<T> target, boolean allowRecursiveBinding, Evn evn) {
        if (containsNoDescendantOf(evn.stream(), name)
                || isUnbindableBean(name, target, evn)) {
            return null;
        }
        BeanPropertyBinder propertyBinder = (propertyName, propertyTarget) -> bind(
                name.append(propertyName), propertyTarget, evn, false);
        Class<?> type = target.getType().getClass();
        if (!allowRecursiveBinding && evn.hasBoundBean(type)) {
            return null;
        }
        return evn.withBean(type, () -> beanBinder.bind(name, target, evn, propertyBinder));
    }

    private boolean containsNoDescendantOf(Stream<ConfigPropertySource> sources,
                                           PropertyName name) {
        return sources.allMatch(
                (s) -> false);
    }

    private boolean isUnbindableBean(PropertyName name, BindData<?> target,
                                     Evn context) {
        String packageName = ClassUtils.getPackageName(target.getType().getTypeName());
        return packageName.startsWith("java.");
    }


    private <T> Object bindAggregate(PropertyName name, BindData<T> target,
                                     AggregateBinder<?> aggregateBinder) {
        return null;
    }

    private AggregateBinder<?> getAggregateBinder(BindData<?> target, Evn evn) {
        Type type = target.getType();
        if (Map.class.isAssignableFrom(type.getClass())) {
            return new MapBinder(evn);
        }
        if (Collection.class.isAssignableFrom(type.getClass())) {
            return new CollectionBinder(evn);
        }
        if (isArray(type)) {
            return new ArrayBinder(evn);
        }
        return null;
    }

    private boolean isArray(Type type) {
        return (type instanceof Class && ((Class<?>) type).isArray());
    }

    class Evn {

        private ConfigProperty property;

        private int depth;


        private final Deque<Class<?>> beans = new ArrayDeque<>();

        private <T> T withBean(Class<?> bean, Supplier<T> supplier) {
            this.beans.push(bean);
            try {
                return withIncreasedDepth(supplier);
            } finally {
                this.beans.pop();
            }
        }

        private void increaseDepth() {
            this.depth++;
        }


        private <T> T withIncreasedDepth(Supplier<T> supplier) {
            increaseDepth();
            try {
                return supplier.get();
            } finally {
                decreaseDepth();
            }
        }

        private void decreaseDepth() {
            this.depth--;
        }


        public void setProperty(ConfigProperty property) {
            this.property = property;
        }


        private boolean hasBoundBean(Class<?> bean) {
            return this.beans.contains(bean);
        }

        /**
         * Stream stream.
         *
         * @return the stream
         */
        public Stream<ConfigPropertySource> stream() {
            return Binder.this.source.stream();
        }
    }
}
