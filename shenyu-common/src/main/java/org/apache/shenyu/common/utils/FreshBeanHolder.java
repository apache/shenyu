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

package org.apache.shenyu.common.utils;

import java.util.function.Function;

public class FreshBeanHolder<E, O> implements Function<E, O> {

    private final Function<E, O> function;

    private volatile O o;

    public FreshBeanHolder(final Function<E, O> function) {
        this.function = function;
    }

    /**
     * Apply.
     *
     * @param e e
     * @return O o
     */
    @Override
    public O apply(final E e) {
        if (o != null) {
            return o;
        }
        return init(e);
    }

    /**
     * Init.
     *
     * @return bean
     */
    synchronized O init(final E e) {
        if (o != null) {
            return o;
        }
        O res = function.apply(e);
        o = res;
        return res;
    }

    /**
     * Fresh.
     *
     * @param e e
     */
    public void doFresh(final E e) {
        O fresh = function.apply(e);
        if (fresh != null) {
            synchronized (this) {
                this.o = fresh;
            }
        }
    }
}
