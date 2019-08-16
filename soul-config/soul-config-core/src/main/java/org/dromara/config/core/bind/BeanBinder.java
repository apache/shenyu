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

/**
 * BeanBinder .
 * java bean binder.
 *
 * @author sixh
 */
public abstract class BeanBinder {

    /**
     * Return a bound bean instance or {@code null} if the {@link BeanBinder} does not
     * support the specified {@link BindData}.
     *
     * @param <T>            The source type
     * @param name           the name being bound
     * @param target         the bindable to bind
     * @param env            the env
     * @param propertyBinder property binder
     * @return a bound instance or {@code null}
     */
    abstract <T> T bind(PropertyName name, BindData<T> target, Binder.Env env, PropertyBinder propertyBinder);


    @FunctionalInterface
    interface PropertyBinder {

        /**
         * Bind the given property.
         *
         * @param propertyName the property name (in lowercase dashed form, e.g.
         *                     {@code first-name})
         * @param target       the target bindable
         * @return the bound value or {@code null}
         */
        Object bindProperty(String propertyName, BindData<?> target);

    }

}
