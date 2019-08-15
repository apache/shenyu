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

import org.dromara.config.api.source.ConfigPropertySource;
import org.dromara.config.api.source.PropertyName;

import java.util.function.Supplier;

/**
 * AggregateBinder .
 * <p>
 * <p>
 * 2019-08-13 21:09
 *
 * @param <T> the type parameter
 * @author chenbin sixh
 */
public abstract class AggregateBinder<T> {

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
        Object result = bindAggregate(name, target, elementBinder);
        Supplier<?> inst = target.getInst();
        if (result == null || inst == null) {
            return result;
        }
        return assemble(inst, (T) result);
    }

    /**
     * 是否可以递归处理.
     *
     * @param source the source
     * @return boolean
     */
    protected abstract boolean isAllowRecursiveBinding(
            ConfigPropertySource source);

    /**
     * Bind aggregate object.
     *
     * @param name          the name
     * @param target        the target
     * @param elementBinder the element binder
     * @return the object
     */
    abstract Object bindAggregate(PropertyName name, BindData<?> target, AggregateElementBinder elementBinder);

    /**
     * 给合元素.
     *
     * @param inst       组合数据的原始类型.
     * @param additional 需要给合的原始数据.
     * @return the t
     */
    abstract T assemble(Supplier<?> inst, T additional);

    /**
     * Internal class used to supply the aggregate and cache the value.
     *
     * @param <T> The aggregate type
     */
    protected static class AggregateSupplier<T> {

        private final Supplier<T> supplier;

        private T supplied;

        public AggregateSupplier(Supplier<T> supplier) {
            this.supplier = supplier;
        }

        public T get() {
            if (this.supplied == null) {
                this.supplied = this.supplier.get();
            }
            return this.supplied;
        }

        public boolean wasSupplied() {
            return this.supplied != null;
        }

    }

}
