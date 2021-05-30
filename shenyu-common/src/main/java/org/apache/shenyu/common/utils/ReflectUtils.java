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

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * The type Reflect utils.
 */
@Slf4j
public class ReflectUtils {

    /**
     * Gets field.
     *
     * @param beanClass the bean class
     * @param name      the name
     * @return the field
     * @throws SecurityException the security exception
     */
    public static Field getField(final Class<?> beanClass, final String name) throws SecurityException {
        final Field[] fields = beanClass.getDeclaredFields();
        if (fields.length != 0) {
            for (Field field : fields) {
                if (name.equals(field.getName())) {
                    return field;
                }
            }
        }
        return null;
    }

    /**
     * Get field value object.
     *
     * @param obj       the obj
     * @param fieldName the field name
     * @return the object
     */
    public static Object getFieldValue(final Object obj, final String fieldName) {
        if (null == obj || StringUtils.isBlank(fieldName)) {
            return null;
        }
        return getFieldValue(obj, getField(obj.getClass(), fieldName));
    }

    /**
     * Gets field value.
     *
     * @param obj   the obj
     * @param field the field
     * @return the field value
     */
    public static Object getFieldValue(final Object obj, final Field field) {
        if (null == obj || null == field) {
            return null;
        }
        field.setAccessible(true);
        Object result = null;
        try {
            result = field.get(obj);
        } catch (IllegalAccessException e) {
            log.error("", e);
        }
        return result;
    }

    /**
     * Invoke method by class.
     *
     * @param clazz  class type
     * @param method method
     * @return Method object
     */
    public static Object invokeMethod(final Class<?> clazz, final String method) {
        try {
            Method m = findMethod(clazz, method);
            return m.invoke(null);
        } catch (Exception e) {
            log.error("", e);
        }

        return null;
    }

    /**
     * Get method by class.
     *
     * @param clazz  class type
     * @param method method
     * @return Method object
     */
    public static Method findMethod(final Class<?> clazz, final String method) {
        try {
            return clazz.getMethod(method);
        } catch (Exception e) {
            log.error("", e);
        }

        return null;
    }
}
