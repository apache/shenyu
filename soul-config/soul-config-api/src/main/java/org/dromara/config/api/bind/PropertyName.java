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

package org.dromara.config.api.bind;

import lombok.Data;
import org.dromara.soul.common.exception.SoulException;

import java.util.ArrayList;
import java.util.List;

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

    private PropertyName(String[] elements) {
        this.elements = elements;
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

    private static void processElement(ElementProcessor processor, CharSequence name,
                                       int start, int end, boolean indexed) {
        if ((end - start) >= 1) {
            processor.process(name.subSequence(start, end), start, end, indexed);
        }
    }

    @FunctionalInterface
    private interface ElementProcessor {

        void process(CharSequence elementValue, int start, int end, boolean indexed);

    }
}
