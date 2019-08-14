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

/**
 * BeanBinder .
 * <p>
 * <p>
 * 2019-08-13 21:09
 *
 * @author chenbin sixh
 */
public interface BeanBinder {
    /**
     * Return a bound bean instance or {@code null} if the {@link BeanBinder} does not
     * support the specified {@link Bindable}.
     *
     * @param <T>            The source type
     * @param name           the name being bound
     * @param target         the bindable to bind
     * @param evn            the evn
     * @param propertyBinder property binder
     * @return a bound instance or {@code null}
     */
    <T> T bind(PropertyName name, BindData<T> target, Binder.Evn evn,
               BeanPropertyBinder propertyBinder);

}
