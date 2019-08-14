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

import lombok.Data;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.StringUtils;

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
@Data
public class PropertyName {

    private String name;

    private String[] elements;

    public static final PropertyName EMPTY = new PropertyName(
            new String[0]);

    private PropertyName(String[] elements) {
        this.elements = elements;
    }

    public String getName() {
        if (name == null) {
            name = toString(this.elements);
        }
        return name;
    }

    private String toString(CharSequence[] elements) {
        StringBuilder result = new StringBuilder();
        for (CharSequence element : elements) {
            boolean indexed = isIndexed(element);
            if (result.length() > 0 && !indexed) {
                result.append(".");
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


    public static PropertyName of(String name) {
        if (name.length() >= 1
                && (name.charAt(0) == '.' || name.charAt(name.length() - 1) == '.')) {
            throw new SoulException("abc");
        }
        if (name.length() == 0) {
            return new PropertyName(new String[0]);
        }
        List<CharSequence> elements = new ArrayList<>(10);
        process(name, '.', (elementValue, start, end, indexed) -> {
            if (elementValue.length() > 0) {
                elements.add(elementValue);
            }
        });
        return new PropertyName(elements.toArray(new String[0]));
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
    static PropertyName adapt(String name, char separator,
                              Function<String, String> elementValueProcessor) {
        if (name.length() == 0) {
            return EMPTY;
        }
        List<String> elements = new ArrayList<>();
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
        return new PropertyName(elements.toArray(new String[0]));
    }


    private static boolean isIndexed(CharSequence element) {
        return element.charAt(0) == '[' && element.charAt(element.length() - 1) == ']';
    }

    private static void process(String name, char separator,
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

    private static void processElement(ElementProcessor processor, String name,
                                       int start, int end, boolean indexed) {
        if ((end - start) >= 1) {
            processor.process(name.substring(start, end), start, end, indexed);
        }
    }

    /**
     * Create a new {@link ConfigurationPropertyName} by appending the given element
     * value.
     *
     * @param elementValue the single element value to append
     * @return a new {@link ConfigurationPropertyName}
     * @throws InvalidConfigurationPropertyNameException if elementValue is not valid
     */
    public PropertyName append(String elementValue) {
        if (elementValue == null) {
            return this;
        }
        process(elementValue, '.', (value, start, end, indexed) -> {
            if (start == 0) {

            }
        });
        if (!isIndexed(elementValue)) {

        }
        int length = this.elements.length;
        String[] elements = new String[length + 1];
        System.arraycopy(this.elements, 0, elements, 0, length);
        elements[length] = elementValue;
        CharSequence[] uniformElements = new CharSequence[length + 1];
        System.arraycopy(this.elements, 0, uniformElements, 0, length);
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

    public boolean isEmpty() {
        return StringUtils.isBlank(name) || elements.length <= 0;
    }

    @FunctionalInterface
    private interface ElementProcessor {

        void process(String elementValue, int start, int end, boolean indexed);

    }
}
