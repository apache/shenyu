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
import org.dromara.config.api.ConfigException;
import org.dromara.config.api.source.ConfigProperty;
import org.dromara.config.api.source.ConfigPropertySource;
import org.dromara.config.api.source.PropertyName;

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
        Env env = new Env();
        return bind(name, target, env, false);
    }

    protected final <T> T bind(PropertyName name, BindData<T> target,
                               Env context, boolean allowRecursiveBinding) {
        Object bound = bindObject(name, target, allowRecursiveBinding, context);
        return (T) bound;

    }

    private <T> Object bindObject(PropertyName name, BindData<T> target,
                                  boolean allowRecursiveBinding, Env env) {
        ConfigProperty property = findProperty(name, env);
        if (property == null && containsNoDescendantOf(env.stream(), name)) {
            return null;
        }
        AggregateBinder<?> aggregateBinder = getAggregateBinder(target, env);
        if (aggregateBinder != null) {
            return bindAggregate(name, target, aggregateBinder, env);
        }
        if (property != null) {
            try {
                return bindProperty(property, env);
            } catch (ConfigException ex) {
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


    private <T> Object bindProperty(ConfigProperty property, Env env) {
        env.setProperty(property);
        return property.getValue();
    }

    private ConfigProperty findProperty(PropertyName name,
                                        Env env) {
        if (name.isEmpty()) {
            return null;
        }
        return env.stream()
                .map((source) -> source.findProperty(name))
                .filter(Objects::nonNull).findFirst().orElse(null);
    }

    private <T> Object bindBean(PropertyName name, BindData<T> target, boolean allowRecursiveBinding, Env env) {
        if (containsNoDescendantOf(env.stream(), name)
                || isUnbindableBean(name, target, env)) {
            return null;
        }
        BeanPropertyBinder propertyBinder = (propertyName, propertyTarget) -> bind(
                name.append(propertyName), propertyTarget, env, false);
        Class<?> type = target.getTypeClass();
        if (!allowRecursiveBinding && env.hasBoundBean(type)) {
            return null;
        }
        return env.withBean(type, () -> beanBinder.bind(name, target, env, propertyBinder));
    }

    private boolean containsNoDescendantOf(Stream<ConfigPropertySource> sources,
                                           PropertyName name) {
        return sources.allMatch(
                (s) -> s.containsDescendantOf(name));
    }

    private boolean isUnbindableBean(PropertyName name, BindData<?> target,
                                     Env env) {
        if (env.stream().anyMatch((s) -> s
                .containsDescendantOf(name))) {
            return false;
        }
        String packageName = ClassUtils.getPackageName(target.getType().getTypeName());
        return packageName.startsWith("java.");
    }


    private <T> Object bindAggregate(PropertyName name, BindData<T> target,
                                     AggregateBinder<?> aggregateBinder, Env env) {
        AggregateElementBinder elementBinder = (itemName, itemTarget, source) -> {
            boolean allowRecursiveBinding = aggregateBinder
                    .isAllowRecursiveBinding(source);
            Supplier<?> supplier = () -> bind(itemName, itemTarget, env,
                    allowRecursiveBinding);
            return env.withSource(source, supplier);
        };
        return env.withIncreasedDepth(
                () -> aggregateBinder.bind(name, target, elementBinder));
    }

    private AggregateBinder<?> getAggregateBinder(BindData<?> target, Env env) {
        Class type = target.getTypeClass();
        if (Map.class.isAssignableFrom(type)) {
            return new MapBinder(env);
        }
        if (Collection.class.isAssignableFrom(type)) {
            return new CollectionBinder(env);
        }
        if (isArray(type)) {
            return new ArrayBinder(env);
        }
        return null;
    }

    private boolean isArray(Type type) {
        return (type instanceof Class && ((Class<?>) type).isArray());
    }

    class Env {

        private ConfigProperty property;

        private int depth;

        private final Deque<Class<?>> beans = new ArrayDeque<>();

        private final List<ConfigPropertySource> source = Arrays
                .asList((ConfigPropertySource) null);

        private int sourcePushCount;

        private <T> T withSource(ConfigPropertySource source,
                                 Supplier<T> supplier) {
            if (source == null) {
                return supplier.get();
            }
            this.source.set(0, source);
            this.sourcePushCount++;
            try {
                return supplier.get();
            } finally {
                this.sourcePushCount--;
            }
        }

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
        Stream<ConfigPropertySource> stream() {
            if (this.sourcePushCount > 0) {
                return this.source.stream();
            }
            return Binder.this.source.stream();
        }

        public List<ConfigPropertySource> getSources() {
            if (this.sourcePushCount > 0) {
                return this.source;
            }
            return Binder.this.source;
        }
    }
}
