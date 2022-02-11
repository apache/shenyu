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

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type Reflect utils.
 */
public class ReflectUtils {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ReflectUtils.class);

    /**
     * Get field.
     *
     * @param beanClass the bean class
     * @param name      the name
     * @return the field
     * @throws SecurityException the security exception
     */
    public static Field getField(final Class<?> beanClass, final String name) throws SecurityException {
        final Field[] fields = beanClass.getDeclaredFields();
        return Arrays.stream(fields).filter(field -> Objects.equals(name, field.getName()))
                .findFirst().orElse(null);
    }

    /**
     * Get field value object.
     *
     * @param obj       the obj
     * @param fieldName the field name
     * @return the object
     */
    public static Object getFieldValue(final Object obj, final String fieldName) {
        if (Objects.isNull(obj) || StringUtils.isBlank(fieldName)) {
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
        if (Objects.isNull(obj) || Objects.isNull(field)) {
            return null;
        }
        field.setAccessible(true);
        Object result = null;
        try {
            result = field.get(obj);
        } catch (IllegalAccessException e) {
            LOG.error("", e);
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
            assert m != null;
            return m.invoke(null);
        } catch (Exception e) {
            LOG.error("", e);
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
            LOG.error("", e);
        }
        return null;
    }

    /**
     * Set object property values directly.
     *
     * @param obj       object
     * @param fieldName tje field name
     * @param value     the field value
     */
    public static void setFieldValue(final Object obj, final String fieldName, final Object value) {
        Field field = getAccessibleField(obj, fieldName);

        if (Objects.isNull(field)) {
            throw new IllegalArgumentException("Could not find field [" + fieldName + "] on target [" + obj + "]");
        }

        try {
            field.set(obj, value);
        } catch (IllegalAccessException e) {
            LOG.error("Failed to assign to the element.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    /**
     * get the object's declared field.
     *
     * @param obj       object
     * @param fieldName tje field name
     * @return {@linkplain Field}
     */
    private static Field getAccessibleField(final Object obj, final String fieldName) {
        Validate.notNull(obj, "object can't be null");
        Validate.notBlank(fieldName, "fieldName can't be blank");
        for (Class<?> superClass = obj.getClass(); superClass != Object.class; superClass = superClass.getSuperclass()) {
            try {
                Field field = superClass.getDeclaredField(fieldName);
                makeAccessible(field);
                return field;
            } catch (NoSuchFieldException e) {
                // Field is not defined in the current class and continues to transition up
                // new add
                LOG.error("field is not defined in the current class and continues to transition up", e);
            }
        }
        return null;
    }

    /**
     * Change the private/protected member variables to public.
     *
     * @param field field
     */
    private static void makeAccessible(final Field field) {
        if ((!Modifier.isPublic(field.getModifiers()) || !Modifier.isPublic(field.getDeclaringClass().getModifiers()) || Modifier
                .isFinal(field.getModifiers())) && !field.isAccessible()) {
            field.setAccessible(true);
        }
    }

    /**
     * Verify the cls is Primitives (Maybe array).
     *
     * @param cls class
     * @return boolean
     */
    public static boolean isPrimitives(final Class<?> cls) {
        return cls.isArray() ? isPrimitive(cls.getComponentType()) : isPrimitive(cls);
    }

    /**
     * Verify the cls is Primitive.
     *
     * @param cls class
     * @return boolean
     */
    public static boolean isPrimitive(final Class<?> cls) {
        return cls.isPrimitive() || cls == String.class || cls == Boolean.class || cls == Character.class
            || Number.class.isAssignableFrom(cls) || Date.class.isAssignableFrom(cls) || List.class.isAssignableFrom(cls);
    }
}
