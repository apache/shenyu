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

package org.dromara.config.core.property;

import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import lombok.Data;

import java.util.*;
import java.util.function.Supplier;

/**
 * PropertyName .
 * <p>
 * <p>
 * 2019-08-15 21:21
 *
 * @author chenbin sixh
 */
@Data
public class PropertyName {

    private final static PropertyName EMPTY = new PropertyName(new String[0]);

    private final static char NAME_JOIN = '.';

    private String name;

    private String[] elements;

    public PropertyName(String[] elements) {
        this.elements = elements;
    }

    public String getName() {
        if (name == null) {
            name = Joiner.on(NAME_JOIN).join(elements);
        }
        return name;
    }

    /**
     * property key 转换为一个PropertyName对象.
     *
     * @param name name;
     * @return this;
     */
    public static PropertyName of(String name) {
        return Optional.ofNullable(name)
                .filter(n -> n.length() > 1)
                .filter(n -> n.charAt(0) != NAME_JOIN && n.charAt(n.length() - 1) != NAME_JOIN)
                .map(n -> {
                    List<String> elements = new ArrayList<>(16);
                    process(n, (e) -> {
                        String element = e.get();
                        if (element.length() > 0) {
                            elements.add(element);
                        }
                    });
                    return new PropertyName(elements.toArray(new String[0]));
                }).orElse(EMPTY);
    }

    private static void process(String element, ElementProcessor processor) {
        Iterable<String> elements = Splitter.on(NAME_JOIN).split(element);
        for (String s : elements) {
            processor.process(() -> s);
        }
    }

    /**
     * 判断当前的这个属性名是否为空.
     * {@link #getElementSize()} > 0
     *
     * @return {@link #elements#getElementSize()}  == 0 true  else false.
     */
    public boolean isEmpty() {
        return this.getElementSize() == 0;
    }

    /**
     * 获取element大小.
     *
     * @return int length.
     */
    public int getElementSize() {
        return this.elements.length;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        PropertyName that = (PropertyName) o;
        return Objects.equals(name, that.name) &&
                Arrays.equals(elements, that.elements);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(name);
        result = 31 * result + Arrays.hashCode(elements);
        return result;
    }

    @FunctionalInterface
    private interface ElementProcessor {

        void process(Supplier<String> element);
    }
}
