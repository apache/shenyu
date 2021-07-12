/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.base.utils;

import java.util.function.Supplier;

/**
 * Bean holder.
 *
 * @param <O> o
 */
public class BeanHolder<O> implements Supplier<O> {

    private Supplier<O> supplier;

    private volatile O o;

    public BeanHolder(final Supplier<O> supplier) {
        this.supplier = supplier;
    }

    /**
     * Get bean.
     *
     * @return bean
     */
    @Override
    public O get() {
        if (o != null) {
            return o;
        }
        return init();
    }

    /**
     * Is null.
     *
     * @return boolean
     */
    public boolean isNull() {
        return o == null;
    }

    /**
     * Init.
     *
     * @return bean
     */
    synchronized O init() {
        if (o != null) {
            return o;
        }
        O res = supplier.get();
        o = res;
        return res;
    }
}
