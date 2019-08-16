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

import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.lang3.ClassUtils;
import org.dromara.config.api.property.ConfigProperty;
import org.dromara.config.api.property.ConfigPropertySource;
import org.dromara.config.api.property.PropertyName;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Supplier;

/**
 * Binder .
 * property binder.
 * 2019-08-15 22:11
 *
 * @author sixh
 */
public class Binder {

    private ConfigPropertySource source;

    private BeanBinder beanBinder = new JavaBeanBinder();

    private Binder(ConfigPropertySource source) {
        this.source = source;
    }

    /**
     * 构造一个属性binder器.
     *
     * @param source source.
     * @return binder binder
     */
    public static Binder of(ConfigPropertySource source) {
        return new Binder(source);
    }

    /**
     * 绑定一个
     *
     * @param <T>          the type parameter
     * @param propertyName the property name
     * @param target       the target
     * @return t t
     */
    public <T> T bind(String propertyName, BindData<T> target) {
        return bind(PropertyName.of(propertyName), target);
    }

    /**
     * Bind t.
     *
     * @param <T>          the type parameter
     * @param propertyName the property name
     * @param target       the target
     * @return the t
     */
    @SuppressWarnings("unchecked")
    public <T> T bind(PropertyName propertyName, BindData<T> target) {
        Env env = new Env();
        return (T) bind(propertyName, target, env, false);
    }

    /**
     * Bind object.
     *
     * @param <T>                   the type parameter
     * @param name                  the name
     * @param target                the target
     * @param env                   the env
     * @param allowRecursiveBinding the allow recursive binding
     * @return the object
     */
    protected final <T> Object bind(PropertyName name, BindData<T> target, Env env, boolean allowRecursiveBinding) {
        return bindObject(name, target, env, allowRecursiveBinding);
    }

    private <T> Object bindObject(PropertyName propertyName, BindData<T> target, Env env, boolean allowRecursiveBinding) {
        ConfigProperty property = findProperty(propertyName, env);

        //查找属性，并判断不是一个属结点.
        if (property == null && env.getSource().containsDescendantOf(propertyName)) {
            return null;
        }
        // 判断是否为aggregate binder.
        AggregateBinder binder = getAggregateBinder(target, env);
        if (binder != null) {
            return bindAggregate(propertyName, target, binder, env);
        }
        if (property != null) {
            try {
                return bindProperty(property, target, env);
            } catch (Exception ex) {
                // We might still be able to bind it as a bean
                Object bean = bindBean(propertyName, target, allowRecursiveBinding, env);
                if (bean != null) {
                    return bean;
                }
                throw ex;
            }

        }
        return bindBean(propertyName, target, allowRecursiveBinding, env);
    }

    private <T> Object bindAggregate(PropertyName name, BindData<T> target,
                                     AggregateBinder<?> aggregateBinder, Env env) {
        AggregateElementBinder elementBinder = (itemName, itemTarget, source) -> {
            boolean allowRecursiveBinding = aggregateBinder.isAllowRecursiveBinding(source);
            Supplier<?> value = () -> bind(itemName, itemTarget, env, allowRecursiveBinding);
            return env.setSource(source.getSource(), value);
        };
        return env.withIncreasedDepth(() -> aggregateBinder.bind(name, target, elementBinder));
    }

    private <T> Object bindProperty(ConfigProperty property, BindData<T> target, Env env) {
        env.setProperty(property);
        Object value = property.getValue();
        return ConvertUtils.convert(value, target.getType().getTypeClass());
    }

    private <T> Object bindBean(PropertyName name, BindData<T> target, boolean allowRecursiveBinding, Env env) {
        if (containsNoDescendantOf(env, name)
                || isUnbindableBean(name, target, env)) {
            return null;
        }
        BeanBinder.PropertyBinder propertyBinder = (propertyName, propertyTarget) -> bind(
                name.append(propertyName), propertyTarget, env, false);
        Class<?> type = target.getType().getTypeClass();
        if (!allowRecursiveBinding && env.hasBoundBean(type)) {
            return null;
        }
        return env.withBean(type, () -> beanBinder.bind(name, target, env, propertyBinder));
    }

    private boolean containsNoDescendantOf(Env env,
                                           PropertyName name) {
        return env.getSource().containsDescendantOf(name);
    }

    private boolean isUnbindableBean(PropertyName name, BindData<?> target,
                                     Env env) {
        if (env.getSource().containsDescendantOf(name)) {
            return false;
        }
        String packageName = ClassUtils.getPackageName(target.getType().getType().getTypeName());
        return packageName.startsWith("java.");
    }

    private AggregateBinder getAggregateBinder(BindData<?> target, Env env) {
        return AggregateBinder.binder(target, env);
    }

    private static ConfigProperty findProperty(PropertyName propertyName, Env env) {
        if (propertyName.isEmpty()) {
            return null;
        }
        return env.getSource().findProperty(propertyName);
    }

    /**
     * The type Env.
     */
    class Env {
        /**
         * sources.
         */
        private ConfigPropertySource source;

        private ConfigProperty property;

        private final Deque<Class<?>> beans = new ArrayDeque<>();

        private int depth;

        private void increaseDepth() {
            this.depth++;
        }

        private void decreaseDepth() {
            this.depth--;
        }

        /**
         * Sets source.
         *
         * @param source the source
         * @param value  the value
         * @return the source
         */
        Object setSource(ConfigPropertySource source, Supplier<?> value) {
            this.source = source;
            return value.get();
        }

        private boolean hasBoundBean(Class<?> bean) {
            return this.beans.contains(bean);
        }

        /**
         * Sets property.
         *
         * @param property the property
         */
        public void setProperty(ConfigProperty property) {
            this.property = property;
        }

        /**
         * Gets source.
         *
         * @return the source
         */
        ConfigPropertySource getSource() {
            if (source == null) {
                return Binder.this.source;
            }
            return source;
        }

        /**
         * Gets property.
         *
         * @return the property
         */
        public ConfigProperty getProperty() {
            return property;
        }

        private <T> T withBean(Class<?> bean, Supplier<T> supplier) {
            this.beans.push(bean);
            try {
                return withIncreasedDepth(supplier);
            } finally {
                this.beans.pop();
            }
        }

        private <T> T withIncreasedDepth(Supplier<T> supplier) {
            increaseDepth();
            try {
                return supplier.get();
            } finally {
                decreaseDepth();
            }
        }
    }
}
