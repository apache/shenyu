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

package org.dromara.config.api.source;

import org.dromara.config.api.ConfigException;
import org.dromara.soul.common.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * PropertyName .
 * <p>
 * <p>
 * 2019-08-13 20:54
 *
 * @author chenbin sixh
 */
public class PropertyName {


    /**
     * The constant EMPTY.
     */
    public static final PropertyName EMPTY = new PropertyName(
            new String[0]);

    private static final char NAME_JOIN = '.';

    private static Logger logger = LoggerFactory.getLogger(PropertyName.class);

    private String name;

    private CharSequence[] elements;

    private PropertyName(CharSequence[] elements) {
        this.elements = elements;
    }

    private PropertyName(String name, CharSequence[] elements) {
        this.elements = elements;
        this.name = name;
    }

    /**
     * Sets name.
     *
     * @param name the name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get elements string [ ].
     *
     * @return the string [ ]
     */
    public CharSequence[] getElements() {
        return elements;
    }


    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        if (name == null) {
            name = toString(this.elements);
        }
        return name;
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
        if (size >= getElementsLength()) {
            return this;
        }
        CharSequence[] elements = new CharSequence[size];
        System.arraycopy(this.elements, 0, elements, 0, size);
        return new PropertyName(elements);
    }

    /**
     * Return if the element in the name is indexed.
     *
     * @param elementIndex the index of the element
     * @return {@code true} if the element is indexed
     */
    boolean isIndexed(int elementIndex) {
        return isIndexed(this.elements[elementIndex]);
    }

    /**
     * Return the last element in the name in the given form.
     *
     * @return the last element
     */
    public String getLastElement() {
        int size = getElementsLength();
        return (size != 0 ? getElement(size - 1) : "");
    }

    /**
     * Return an element in the name in the given form.
     *
     * @param elementIndex the element index
     * @return the last element
     */
    public String getElement(int elementIndex) {
        CharSequence result = this.elements[elementIndex];
        if (isIndexed(result)) {
            result = result.subSequence(1, result.length() - 1);
        }
        return result.toString();
    }


    /**
     * Return if the last element in the name is indexed.
     *
     * @return {@code true} if the last element is indexed
     */
    public boolean isLastElementIndexed() {
        int size = getElementsLength();
        return (size > 0 && isIndexed(this.elements[size - 1]));
    }

    public boolean isParentOf(PropertyName name) {
        if (this.getElementsLength() != name.getElementsLength() - 1) {
            return false;
        }
        return isAncestorOf(name);
    }

    /**
     * Gets elements length.
     *
     * @return the elements length
     */
    public int getElementsLength() {
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
            if (!elementEquals(this.elements[i], name.elements[i])) {
                return false;
            }
        }
        return true;
    }

    private boolean elementEquals(CharSequence e1, CharSequence e2) {
        int l1 = e1.length();
        int l2 = e2.length();
        boolean indexed1 = isIndexed(e1);
        int offset1 = (indexed1 ? 1 : 0);
        boolean indexed2 = isIndexed(e2);
        int offset2 = (indexed2 ? 1 : 0);
        int i1 = offset1;
        int i2 = offset2;
        while (i1 < l1 - offset1) {
            if (i2 >= l2 - offset2) {
                return false;
            }
            char ch1 = (indexed1 ? e1.charAt(i1) : Character.toLowerCase(e1.charAt(i1)));
            char ch2 = (indexed2 ? e2.charAt(i2) : Character.toLowerCase(e2.charAt(i2)));
            if (ch1 == '-' || ch1 == '_') {
                i1++;
            } else if (ch2 == '-' || ch2 == '_') {
                i2++;
            } else if (ch1 != ch2) {
                return false;
            } else {
                i1++;
                i2++;
            }
        }
        while (i2 < l2 - offset2) {
            char ch = e2.charAt(i2++);
            if (ch != '-' && ch != '_') {
                return false;
            }
        }
        return true;
    }

    private String toString(CharSequence[] elements) {
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
                    char ch = Character.toLowerCase(element.charAt(i));
                    result.append(ch != '_' ? ch : "");
                }
            }
        }
        return result.toString();
    }


    /**
     * Of property name.
     *
     * @param name the name
     * @return the property name
     */
    public static PropertyName of(String name) {
        if (StringUtils.isBlank(name)) {
            throw new NullPointerException("name is null");
        }
        int length = name.length();

        if (length >= 1 && (name.charAt(0) == NAME_JOIN || name.charAt(name.length() - 1) == '.')) {
            throw new ConfigException("name is null");
        }
        if (length == 0) {
            return new PropertyName(new String[0]);
        }
        List<CharSequence> elements = new ArrayList<>(10);
        process(name, NAME_JOIN, (elementValue, start, end, indexed) -> {
            if (elementValue.length() > 0) {
                elements.add(elementValue);
            }
        });
        return new PropertyName(name, elements.toArray(new CharSequence[0]));
    }

    /**
     * Create a {@link PropertyName} by adapting the given source. See
     * {@link #adapt(CharSequence, char, Function)} for details.
     *
     * @param name      the name to parse
     * @param separator the separator used to split the name
     * @return a {@link PropertyName}
     */
    static PropertyName adapt(String name, char separator) {
        return adapt(name, separator, Function.identity());
    }

    /**
     * Create a {@link PropertyName} by adapting the given source. The name
     * is split into elements around the given {@code separator}. This method is more
     * lenient than {@link #of} in that it allows mixed case names and '{@code _}'
     * characters. Other invalid characters are stripped out during parsing.
     * <p>
     * The {@code elementValueProcessor} function may be used if additional processing is
     * required on the extracted element values.
     *
     * @param name                  the name to parse
     * @param separator             the separator used to split the name
     * @param elementValueProcessor a function to process element values
     * @return a {@link PropertyName}
     */
    static PropertyName adapt(CharSequence name, char separator,
                              Function<CharSequence, CharSequence> elementValueProcessor) {
        if (name.length() == 0) {
            return EMPTY;
        }
        List<CharSequence> elements = new ArrayList<>();
        process(name, separator, (elementValue, start, end, indexed) -> {
            elementValue = elementValueProcessor.apply(elementValue);
            if (!isIndexed(elementValue)) {
              /*  elementValue = cleanupCharSequence(elementValue,
                        (ch, index) -> ch != '_' && !ElementValidator
                                .isValidChar(Character.toLowerCase(ch), index),
                        CharProcessor.NONE);*/
            }
            if (elementValue.length() > 0) {
                elements.add(elementValue);
            }
        });
        return new PropertyName(elements.toArray(new CharSequence[0]));
    }

    /**
     * Whether the parameter of the index type list array.
     */
    private static boolean isIndexed(CharSequence element) {
        return element.charAt(0) == '[' && element.charAt(element.length() - 1) == ']';
    }

    private static void process(CharSequence name, char separator,
                                ElementProcessor processor) {
        int start = 0;
        boolean indexed = false;
        int length = name.length();
        int openBracketCount = 0;
        for (int i = 0; i < length; i++) {
            char ch = name.charAt(i);
            if (ch == ']') {
                openBracketCount--;
                if (openBracketCount == 0) {
                    processElement(processor, name, start, i + 1, indexed);
                    start = i + 1;
                    indexed = false;
                }
            } else if (ch == '[') {
                openBracketCount++;
                if (!indexed) {
                    processElement(processor, name, start, i, indexed);
                    start = i;
                    indexed = true;
                }
            } else if (!indexed && ch == separator) {
                processElement(processor, name, start, i, indexed);
                start = i + 1;
            }
        }
        processElement(processor, name, start, length, false);
    }

    private static void processElement(ElementProcessor processor, CharSequence name,
                                       int start, int end, boolean indexed) {
        if ((end - start) >= 1) {
            processor.process(name.subSequence(start, end), start, end, indexed);
        }
    }

    /**
     * Append property name.
     *
     * @param elementValue the element value
     * @return the property name
     */
    public PropertyName append(CharSequence elementValue) {
        if (elementValue == null) {
            return this;
        }
        process(elementValue, NAME_JOIN, (value, start, end, indexed) -> {
            if (start == 0) {
                logger.warn("{} Did not find the corresponding property.", elementValue);
            }
        });
        if (!isIndexed(elementValue)) {
            List<Character> invalidChars = ElementValidator.getInvalidChars(elementValue);
            if (!invalidChars.isEmpty()) {
                throw new ConfigException("config property name " + elementValue + " is not valid");
            }
        }
        int length = this.elements.length;
        CharSequence[] elements = new String[length + 1];
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

    /**
     * Is empty boolean.
     *
     * @return the boolean
     */
    public boolean isEmpty() {
        return elements.length <= 0;
    }

    @FunctionalInterface
    private interface ElementProcessor {

        /**
         * Process.
         *
         * @param elementValue the element value
         * @param start        the start
         * @param end          the end
         * @param indexed      the indexed
         */
        void process(CharSequence elementValue, int start, int end, boolean indexed);

    }

    /**
     * {@link ElementProcessor} that checks if a name is valid.
     */
    private static class ElementValidator implements ElementProcessor {

        private boolean valid = true;

        @Override
        public void process(CharSequence elementValue, int start, int end,
                            boolean indexed) {
            if (this.valid && !indexed) {
                this.valid = isValidElement(elementValue);
            }
        }

        /**
         * Is valid boolean.
         *
         * @return the boolean
         */
        public boolean isValid() {
            return this.valid;
        }

        /**
         * Is valid element boolean.
         *
         * @param elementValue the element value
         * @return the boolean
         */
        public static boolean isValidElement(CharSequence elementValue) {
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
        public static List<Character> getInvalidChars(CharSequence elementValue) {
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
        public static boolean isValidChar(char ch, int index) {
            return isAlpha(ch) || isNumeric(ch) || (index != 0 && ch == '-');
        }

        private static boolean isAlpha(char ch) {
            return ch >= 'a' && ch <= 'z';
        }

        private static boolean isNumeric(char ch) {
            return ch >= '0' && ch <= '9';
        }

    }
}
