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

package org.dromara.soul.common.utils;

import java.util.*;

/**
 * CollectionUtils .
 * collection 相关的处理.
 *
 * @author sixh
 */
public class CollectionUtils {

    /**
     * Create factory collection factory.
     *
     * @return the collection factory
     */
    public static CollectionFactory createFactory() {
        return new CollectionFactory();
    }

    /**
     * Is empty boolean.
     *
     * @param coll the coll
     * @return the boolean
     */
    public static boolean isEmpty(Collection<?> coll) {
        return coll == null || coll.isEmpty();
    }

    /**
     * Is not empty boolean.
     *
     * @param coll the coll
     * @return the boolean
     */
    public static boolean isNotEmpty(Collection<?> coll) {
        return !isEmpty(coll);
    }

    /**
     * The type Collection factory.
     */
    public static class CollectionFactory {

        /**
         * Create collection.
         *
         * @param <E>            the type parameter
         * @param collectionType the collection type
         * @param capacity       the capacity
         * @return the collection
         */
        public <E> Collection<E> create(Class<?> collectionType, int capacity) {
            return create(collectionType, null, capacity);
        }

        /**
         * Create collection.
         *
         * @param <E>            the type parameter
         * @param collectionType the collection type
         * @param elementType    the element type
         * @param capacity       the capacity
         * @return the collection
         */
        @SuppressWarnings({"unchecked", "rawtypes"})
        public <E> Collection<E> create(Class<?> collectionType, Class<?> elementType, int capacity) {
            if (collectionType.isInterface()) {
                if (Set.class == collectionType || Collection.class == collectionType) {
                    return new LinkedHashSet<>(capacity);
                } else if (List.class == collectionType) {
                    return new ArrayList<>(capacity);
                } else if (SortedSet.class == collectionType || NavigableSet.class == collectionType) {
                    return new TreeSet<>();
                } else {
                    throw new IllegalArgumentException("Unsupported Collection interface: " + collectionType.getName());
                }
            } else if (EnumSet.class == collectionType) {
                // Cast is necessary for compilation in Eclipse 4.4.1.
                return (Collection<E>) EnumSet.noneOf(asEnumType(elementType));
            } else {
                if (!Collection.class.isAssignableFrom(collectionType)) {
                    throw new IllegalArgumentException("Unsupported Collection type: " + collectionType.getName());
                }
                try {
                    return (Collection<E>) collectionType.newInstance();
                } catch (Throwable ex) {
                    throw new IllegalArgumentException(
                            "Could not instantiate Collection type: " + collectionType.getName(), ex);
                }
            }
        }

        /**
         * Create map map.
         *
         * @param <K>      the type parameter
         * @param <V>      the type parameter
         * @param mapType  the map type
         * @param capacity the capacity
         * @return the map
         */
        public <K, V> Map<K, V> createMap(Class<?> mapType, int capacity) {
            return createMap(mapType, null, capacity);
        }

        /**
         * Cast the given type to a subtype of {@link Enum}.
         *
         * @param enumType the enum type, never {@code null}
         * @return the given type as subtype of {@link Enum}
         * @throws IllegalArgumentException if the given type is not a subtype of {@link Enum}
         */
        @SuppressWarnings("rawtypes")
        private Class<? extends Enum> asEnumType(Class<?> enumType) {
            if (!Enum.class.isAssignableFrom(enumType)) {
                throw new IllegalArgumentException("Supplied type is not an enum: " + enumType.getName());
            }
            return enumType.asSubclass(Enum.class);
        }

        /**
         * Create map map.
         *
         * @param <K>     the type parameter
         * @param <V>     the type parameter
         * @param mapType the map type
         * @param keyType the key type
         * @param size    the size
         * @return the map
         */
        public <K, V> Map<K, V> createMap(Class<?> mapType, Class<?> keyType, int size) {
            if (mapType.isInterface()) {
                if (Map.class == mapType) {
                    return new LinkedHashMap<>(size);
                } else if (SortedMap.class == mapType || NavigableMap.class == mapType) {
                    return new TreeMap<>();
                } else {
                    throw new IllegalArgumentException("Unsupported Map interface: " + mapType.getName());
                }
            } else if (EnumMap.class == mapType) {
                return new EnumMap(asEnumType(keyType));
            } else {
                if (!Map.class.isAssignableFrom(mapType)) {
                    throw new IllegalArgumentException("Unsupported Map type: " + mapType.getName());
                }
                try {
                    return (Map<K, V>) mapType.newInstance();
                } catch (Throwable ex) {
                    throw new IllegalArgumentException("Could not instantiate Map type: " + mapType.getName(), ex);
                }
            }
        }
    }
}
