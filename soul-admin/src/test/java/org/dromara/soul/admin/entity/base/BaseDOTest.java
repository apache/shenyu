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

package org.dromara.soul.admin.entity.base;

import org.junit.jupiter.api.Test;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * base class for test.
 *
 * @author lw1243925457
 */
public abstract class BaseDOTest<T> {

    protected abstract T getTargetClass();

    /**
     * invoke getter and setter method by reflect.
     *
     * @throw Exception maybe throw reflect Exception.
     */
    private void invokeGetAndSet() throws Exception {
        T targetClass = getTargetClass();

        Class<?> clazz = targetClass.getClass();
        Object obj = clazz.getDeclaredConstructor().newInstance();
        Field[] fields = clazz.getDeclaredFields();

        for (Field f : fields) {
            if (f.isSynthetic()) {
                continue;
            }

            PropertyDescriptor pd = new PropertyDescriptor(f.getName(), clazz);
            Method get = pd.getReadMethod();
            Method set = pd.getWriteMethod();

            set.invoke(obj, get.invoke(obj));
        }
    }

    @Test
    void shouldSuccessGetAndSetMethod() throws Exception {
        invokeGetAndSet();
    }
}
