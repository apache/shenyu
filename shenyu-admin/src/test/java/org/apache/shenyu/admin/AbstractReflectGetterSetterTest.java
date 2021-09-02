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

package org.apache.shenyu.admin;

import org.junit.jupiter.api.Test;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Abstract test class for getter and setter.
 */
public abstract class AbstractReflectGetterSetterTest {

    /**
     * A map of default mappers for common objects.
     */
    private static final Map<Class<?>, Object> DEFAULT_MAPPERS;

    static {
        DEFAULT_MAPPERS = new HashMap<>();

        /* Primitives */
        DEFAULT_MAPPERS.put(int.class, 0);
        DEFAULT_MAPPERS.put(double.class, 0.0d);
        DEFAULT_MAPPERS.put(float.class, 0.0f);
        DEFAULT_MAPPERS.put(long.class, 0L);
        DEFAULT_MAPPERS.put(boolean.class, true);
        DEFAULT_MAPPERS.put(short.class, 0);
        DEFAULT_MAPPERS.put(byte.class, 0);
        DEFAULT_MAPPERS.put(char.class, 0);

        DEFAULT_MAPPERS.put(Integer.class, 0);
        DEFAULT_MAPPERS.put(Double.class, 0.0);
        DEFAULT_MAPPERS.put(Float.class, 0.0f);
        DEFAULT_MAPPERS.put(Long.class, 0L);
        DEFAULT_MAPPERS.put(Boolean.class, Boolean.TRUE);
        DEFAULT_MAPPERS.put(Short.class, (short) 0);
        DEFAULT_MAPPERS.put(Byte.class, (byte) 0);
        DEFAULT_MAPPERS.put(Character.class, (char) 0);

        DEFAULT_MAPPERS.put(BigDecimal.class, BigDecimal.ONE);
        DEFAULT_MAPPERS.put(Date.class, new Date());
        DEFAULT_MAPPERS.put(Timestamp.class, new Timestamp(System.currentTimeMillis()));

        /* Collection Types. */
        DEFAULT_MAPPERS.put(Set.class, Collections.emptySet());
        DEFAULT_MAPPERS.put(SortedSet.class, Collections.emptySortedSet());
        DEFAULT_MAPPERS.put(List.class, Collections.emptyList());
        DEFAULT_MAPPERS.put(Map.class, Collections.emptyMap());
        DEFAULT_MAPPERS.put(SortedMap.class, Collections.emptySortedMap());
    }

    protected abstract Class<?> getTargetClass();

    protected Set<String> getExcludeFields() {
        return new HashSet<>(Collections.singletonList("serialVersionUID"));
    }

    /**
     * Test getter and setter method by reflect.
     *
     * @throws Exception maybe throw reflect Exception.
     */
    @Test
    public void testGetAndSet() throws Exception {
        Class<?> clazz = getTargetClass();
        Object target = clazz.getDeclaredConstructor().newInstance();
        Field[] fields = clazz.getDeclaredFields();
        Set<String> excludeFields = getExcludeFields();

        Stream.of(fields)
                .forEach(f -> {
                    if (f.isSynthetic()) {
                        return;
                    }
                    if (excludeFields != null && excludeFields.contains(f.getName())) {
                        return;
                    }
                    try {
                        // get the get and set methods of the field by PropertyDescriptor
                        // (String) f.getName() for java11 (can not find com.sun.beans.introspect.PropertyInfo class)
                        PropertyDescriptor property = new PropertyDescriptor(f.getName(), clazz);
                        Method getter = property.getReadMethod();
                        Method setter = property.getWriteMethod();

                        final Object setValue = defaultValue(property.getPropertyType());

                        setter.invoke(target, setValue);
                        final Object getValue = getter.invoke(target);
                        assertEquals(setValue, getValue,
                                property.getDisplayName() + " getter / setter do not produce the same result."
                        );
                    } catch (IntrospectionException | IllegalAccessException | InvocationTargetException | InstantiationException e) {
                        throw new RuntimeException("", e);
                    }
                });
    }

    private Object defaultValue(final Class<?> clazz) throws IllegalAccessException, InstantiationException {
        final Object obj = DEFAULT_MAPPERS.get(clazz);
        if (obj != null) {
            return obj;
        }

        return clazz.newInstance();
    }
}
