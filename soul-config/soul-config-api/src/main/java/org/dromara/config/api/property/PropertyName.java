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

package org.dromara.config.api.property;

import com.google.common.base.Splitter;
import lombok.Data;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private static final Logger logger = LoggerFactory.getLogger(PropertyName.class);

    private static final PropertyName EMPTY = new PropertyName(new String[0]);

    private static final char NAME_JOIN = '.';

    private String name;

    private String[] elements;

    /**
     * Instantiates a new Property name.
     *
     * @param elements the elements
     */
    public PropertyName(String[] elements) {
        this.elements = elements;
    }

    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        if (name == null) {
            name = toName();
        }
        return name;
    }

    private String toName() {
        StringBuilder result = new StringBuilder();
        for (CharSequence element : elements) {
            boolean indexed = isIndexed(element);
            if (result.length() > 0 && !indexed) {
                result.append(NAME_JOIN);
            }
            if (indexed) {
                result.append(element);
            } else {
                for (int i = 0; i < element.length(); i++) {
                    char ch = element.charAt(i);
                    result.append(ch != '_' ? ch : "");
                }
            }
        }
        return result.toString();
    }

    /**
     * property key 转换为一个PropertyName对象.
     *
     * @param name name;
     * @return this ;
     */
    public static PropertyName of(String name) {
        return Optional.ofNullable(name)
                .filter(n -> n.length() > 1)
                .filter(n -> n.charAt(0) != NAME_JOIN && n.charAt(n.length() - 1) != NAME_JOIN)
                .map(n -> {
                    List<String> elements = new ArrayList<>(16);
                    process(n, (e, indexed) -> {
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
            if (possibleIsIndexed(s)) {
                String preElement = s.substring(0, s.indexOf("["));
                String postElement = s.substring(s.indexOf("["));
                processor.process(() -> preElement, isIndexed(s));
                processor.process(() -> postElement, isIndexed(s));
            } else {
                processor.process(() -> s, isIndexed(s));
            }
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

    /**
     * Determine if the node name of the pointing is a parent. If yes, return true.
     *
     * @param name name.
     * @return boolean boolean
     */
    public boolean isAncestorOf(PropertyName name) {

        if (this.getElements().length >= name.getElements().length) {
            return false;
        }
        for (int i = 0; i < this.elements.length; i++) {
            if (!Objects.equals(this.elements[i], name.elements[i])) {
                return false;
            }
        }
        return true;
    }

    public boolean isParentOf(PropertyName name) {
        if (this.getElementSize() != name.getElementSize() - 1) {
            return false;
        }
        return isAncestorOf(name);
    }


    /**
     * Return if the element in the name is indexed and numeric.
     *
     * @param elementIndex the index of the element
     * @return {@code true} if the element is indexed and numeric
     */
    public boolean isNumericIndex(int elementIndex) {
        return isIndexed(elementIndex)
                && isNumeric(getElement(elementIndex));
    }

    private boolean isNumeric(CharSequence element) {
        for (int i = 0; i < element.length(); i++) {
            if (!Character.isDigit(element.charAt(i))) {
                return false;
            }
        }
        return true;
    }


    /**
     * Return a new {@link PropertyName} by chopping this name to the given
     * {@code size}. For example, {@code chop(1)} on the name {@code foo.bar} will return
     * {@code foo}.
     *
     * @param size the size to chop
     * @return the chopped name
     */
    public PropertyName chop(int size) {
        if (size >= getElementSize()) {
            return this;
        }
        String[] elements = new String[size];
        System.arraycopy(this.elements, 0, elements, 0, size);
        return new PropertyName(elements);
    }

    /**
     * 是否为最后一个元素.
     *
     * @return boolean. boolean
     */
    public boolean isLastElementIndexed() {
        int elementSize = getElementSize();
        return elementSize > 0 && isIndexed(this.getElements()[elementSize - 1]);
    }

    /**
     * Gets last element.
     *
     * @return the last element
     */
    public String getLastElement() {
        int elementSize = getElementSize();
        return (elementSize != 0 ? getElement(elementSize - 1) : "");
    }

    /**
     * Gets element.
     *
     * @param index the index
     * @return the element
     */
    public String getElement(int index) {
        return getElements()[index];
    }

    /**
     * Whether the parameter of the index type list array.
     */
    private static boolean isIndexed(CharSequence element) {
        return element.charAt(0) == '[' && possibleIsIndexed(element);
    }

    private static boolean possibleIsIndexed(CharSequence element) {
        return element.charAt(element.length() - 1) == ']';
    }

    /**
     * Whether the parameter of the index type list array.
     */
    private boolean isIndexed(int index) {
        String element = getElement(index);
        return isIndexed(element);
    }

    /**
     * Append property name.
     *
     * @param elementValue the element value
     * @return the property name
     */
    public PropertyName append(String elementValue) {
        if (elementValue == null) {
            return this;
        }
        process(elementValue, (e, indexed) -> {
            if (StringUtils.isBlank(e.get()) && logger.isDebugEnabled()) {
                logger.debug("{} Did not find the corresponding property.", elementValue);
            }
        });
        if (!isIndexed(elementValue)) {
            List<Character> invalidChars = ElementValidator.getInvalidChars(elementValue);
            if (!invalidChars.isEmpty()) {
                throw new SoulException("config property name " + elementValue + " is not valid");
            }
        }
        int length = this.elements.length;
        String[] elements = new String[length + 1];
        System.arraycopy(this.elements, 0, elements, 0, length);
        elements[length] = elementValue;
        return new PropertyName(elements);
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

        /**
         * Process.
         *
         * @param element the element
         */
        void process(Supplier<String> element, boolean indexed);
    }


    /**
     * {@link ElementProcessor} that checks if a name is valid.
     */
    private static class ElementValidator implements ElementProcessor {

        private boolean valid = true;

        @Override
        public void process(Supplier<String> element, boolean indexed) {
            if (this.isValid() && !indexed) {
                this.valid = isValidElement(element.get());
            }
        }

        /**
         * Is valid boolean.
         *
         * @return the boolean
         */
        boolean isValid() {
            return this.valid;
        }

        /**
         * Is valid element boolean.
         *
         * @param elementValue the element value
         * @return the boolean
         */
        static boolean isValidElement(CharSequence elementValue) {
            for (int i = 0; i < elementValue.length(); i++) {
                char ch = elementValue.charAt(i);
                if (!isValidChar(ch, i)) {
                    return false;
                }
            }
            return true;
        }

        /**
         * Gets invalid chars.
         *
         * @param elementValue the element value
         * @return the invalid chars
         */
        static List<Character> getInvalidChars(CharSequence elementValue) {
            List<Character> chars = new ArrayList<>();
            for (int i = 0; i < elementValue.length(); i++) {
                char ch = elementValue.charAt(i);
                if (!isValidChar(ch, i)) {
                    chars.add(ch);
                }
            }
            return chars;
        }

        /**
         * Is valid char boolean.
         *
         * @param ch    the ch
         * @param index the index
         * @return the boolean
         */
        static boolean isValidChar(char ch, int index) {
            return isAlpha(ch) || isNumeric(ch) || (index != 0 && ch == '-');
        }

        private static boolean isAlpha(char ch) {
            return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
        }

        private static boolean isNumeric(char ch) {
            return ch >= '0' && ch <= '9';
        }
    }
}
