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

package org.apache.shenyu.admin.utils;

import org.apache.commons.collections4.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * ListUtil.
 */
public final class ListUtil {
    
    private ListUtil() {
    }
    
    /**
     * list.
     *
     * @param t   e
     * @param <T> type
     * @return list
     */
    @SafeVarargs
    public static <T> List<T> list(final T... t) {
        return Stream.of(t).collect(Collectors.toList());
    }
    
    /**
     * new array list.
     *
     * @param t   e
     * @param <T> type
     * @return list
     */
    @SafeVarargs
    public static <T> List<T> of(final T... t) {
        return new ArrayList<>(list(t));
    }
    
    /**
     * if list is empty, return default value.
     *
     * @param list     list
     * @param defaultV default value
     * @param <T>      list type
     * @return default value or list
     */
    public static <T> List<T> emptyIsDefault(final List<T> list, final List<T> defaultV) {
        return CollectionUtils.isEmpty(list) ? defaultV : list;
    }
    
    /**
     * list map.<br>
     * if element is null it filter.
     *
     * @param list     list
     * @param function map fun
     * @param <R>      resource type
     * @param <T>      target type
     * @return list
     */
    public static <R, T> List<T> map(final Collection<R> list, final Function<? super R, ? extends T> function) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        return list.stream()
                .filter(Objects::nonNull)
                .map(function)
                .collect(Collectors.toList());
    }
    
    /**
     * list map.<br>
     * if element is null it filter.
     *
     * @param list     list
     * @param function map fun
     * @param <R>      resource type
     * @return list
     */
    public static <R> R findFirst(final List<R> list, final Function<R, Boolean> function) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        for (R r : list) {
            if (Boolean.TRUE.equals(function.apply(r))) {
                return r;
            }
        }
        return null;
    }
    
    /**
     * list to map.<br>
     * if element is null it filters.
     *
     * @param list     list
     * @param function map fun
     * @param <K>      map key type
     * @param <U>      map value type
     * @return list
     */
    public static <K, U> Map<K, U> toMap(final Collection<U> list, final Function<? super U, ? extends K> function) {
        return toMap(list, function, Function.identity());
    }
    
    /**
     * list to map.<br>
     * if element is null it filters.
     *
     * @param list        list
     * @param keyMapper   map key convert
     * @param valueMapper map value convert
     * @param <K>         map key type
     * @param <U>         map value type
     * @param <T>         list value type
     * @return list
     */
    public static <K, U, T> Map<K, U> toMap(final Collection<T> list, final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyMap();
        }
        return list.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(keyMapper, valueMapper, (value1, value2) -> value1));
    }
    
    /**
     * list group by.<br>
     * if element is null it filters.
     *
     * @param list     list
     * @param function group key fun
     * @param <K>      map key type
     * @param <U>      map value type
     * @return list
     */
    public static <K, U> Map<K, List<U>> groupBy(final Collection<U> list, final Function<? super U, ? extends K> function) {
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyMap();
        }
        return list.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(function));
    }
    
    /**
     * merge.
     *
     * @param set1 merge to.
     * @param set2 merge from.
     * @param <T>  type
     * @return collection1
     */
    public static <T> Set<T> mergeSet(final Set<T> set1, final Set<T> set2) {
        set1.addAll(set2);
        return set1;
    }
    
    /**
     * merge.
     *
     * @param list1 list1
     * @param list2 list2
     * @param <T>   type
     * @return list1
     */
    public static <T> List<T> merge(final List<T> list1, final List<T> list2) {
        list1.addAll(list2);
        return list1;
    }
}
