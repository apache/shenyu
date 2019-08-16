/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.core.bind;

import org.dromara.config.core.property.PropertyName;

import java.util.function.Supplier;

/**
 * AggregateBinder .
 * 关于聚合内的binder操作.
 *
 * @author sixh
 * @see java.util.Map
 * @see java.lang.reflect.Array
 * @see java.util.Collection
 */
public abstract class AggregateBinder<T> {

    /**
     * The Env.
     */
    private Binder.Env env;

    /**
     * Gets env.
     *
     * @return the env
     */
    public Binder.Env getEnv() {
        return env;
    }

    /**
     * Instantiates a new Aggregate binder.
     *
     * @param env the env
     */
    public AggregateBinder(Binder.Env env) {
        this.env = env;
    }

    /**
     * Bind object.
     *
     * @param name          the name
     * @param target        the target
     * @param elementBinder the element binder
     * @return the object
     */
    @SuppressWarnings("unchecked")
    public Object bind(PropertyName name, BindData<?> target, AggregateElementBinder elementBinder) {
        Object result = bind(name, target, getEnv(), elementBinder);
        Supplier<?> targetValue = target.getValue();
        if (result == null || targetValue == null) {
            return result;
        }
        return merge(targetValue, (T)result);
    }

    /**
     * AggregateBinder Bind object.
     *
     * @param propertyName  the property name
     * @param target        the target
     * @param env           the env
     * @param elementBinder the element binder
     * @return the object
     */
    abstract Object bind(PropertyName propertyName, BindData<?> target, Binder.Env env, AggregateElementBinder elementBinder);

    /**
     * Merge object.
     *
     * @param targetValue the target value
     * @param object      the object
     * @return the object
     */
    abstract Object merge(Supplier<?> targetValue, T object);

    /**
     * 获取一个聚合的绑定器.
     *
     * @param target ta
     * @param env    the env
     * @return aggregate binder
     */
    static AggregateBinder binder(BindData<?> target, Binder.Env env) {
        DataType type = target.getType();
        //如果map集合.
        if (type.isMap()) {
            return new MapBinder(env);
        } else if (type.isCollection()) {
            return new CollectionBinder(env);
        } else if (type.isArray()) {
            return new ArrayBinder(env);
        }
        return null;
    }

    public abstract boolean isAllowRecursiveBinding(Binder.Env source);

    /**
     * Internal class used to supply the aggregate and cache the value.
     *
     * @param <T> The aggregate type
     */
    protected static class AggregateSupplier<T> {

        private final Supplier<T> supplier;

        private T supplied;

        /**
         * Instantiates a new Aggregate supplier.
         *
         * @param supplier the supplier
         */
        public AggregateSupplier(Supplier<T> supplier) {
            this.supplier = supplier;
        }

        /**
         * Get t.
         *
         * @return the t
         */
        public T get() {
            if (this.supplied == null) {
                this.supplied = this.supplier.get();
            }
            return this.supplied;
        }

        /**
         * Was supplied boolean.
         *
         * @return the boolean
         */
        public boolean wasSupplied() {
            return this.supplied != null;
        }

    }

}
