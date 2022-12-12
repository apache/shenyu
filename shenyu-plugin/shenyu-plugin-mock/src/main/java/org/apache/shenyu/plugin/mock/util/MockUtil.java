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

package org.apache.shenyu.plugin.mock.util;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.shenyu.plugin.mock.generator.CurrentTimeGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Objects;
import java.util.Random;

import static org.apache.shenyu.plugin.mock.util.RandomUtil.randomLowerLetterString;

public class MockUtil {

    private static final Logger LOG = LoggerFactory.getLogger(CurrentTimeGenerator.class);

    private static final String DEFAULT_FORMAT = "YYYY-MM-dd HH:mm:ss";

    private static final String[] DOMAIN_SUFFIX = {"com", "org", "cn", "com.cn", "top", "edu", "io"};

    /**
     * Randomly generate Boolean.
     *
     * @return Boolean
     */
    public static Boolean bool() {
        return RandomUtil.randomInt(0, 1) == 1;
    }

    /**
     * Randomly generate int in the specified range.
     *
     * @param min min
     * @param max max
     * @return int
     */
    public static int randomInt(final int min, final int max) {
        return RandomUtil.randomInt(min, max);
    }

    /**
     * Randomly generate Double in the specified range.
     *
     * @param min    min
     * @param max    max
     * @param format format
     * @return formatDouble
     */
    public static FormatDouble randomDouble(final double min, final double max, final String... format) {

        Double result = (Math.random() * (max - min)) + min;
        if (format != null && format.length != 0) {
            return new FormatDouble(result, format[0]);
        }
        return new FormatDouble(result);
    }

    /**
     * Randomly generate email.
     *
     * @return email
     */
    public static String email() {
        return String.format("%s@%s.%s",
                randomLowerLetterString(randomInt(5, 10)),
                randomLowerLetterString(randomInt(3, 8)),
                DOMAIN_SUFFIX[randomInt(0, DOMAIN_SUFFIX.length - 1)]);
    }

    /**
     * Randomly generate phone.
     *
     * @return phone
     */
    public static String phone() {
        StringBuilder builder = new StringBuilder("1");
        builder.append(RandomUtil.randomInt(3, 9));
        for (int i = 0; i < 9; i++) {
            builder.append(RandomUtil.randomInt(0, 9));
        }
        return builder.toString();
    }

    /**
     * Randomly generate Chinese string.
     *
     * @param min min
     * @param max max
     * @return chinese string
     */
    public static String zh(final int min, final int max) {
        Random random = new Random();
        int len = random.nextInt(max - min - 1) + min;
        return RandomStringUtils.random(len, 0x4e00, 0x9fa5, false, false);
    }

    /**
     * Randomly generate English string.
     *
     * @param min min
     * @param max max
     * @return english string
     */
    public static String en(final int min, final int max) {
        return RandomStringUtils.random(RandomUtil.randomInt(min, max), 5, 129, true, false);
    }

    /**
     * Randomly generate item of data.
     *
     * @param data data
     * @return item
     */
    public static Object oneOf(final Object... data) {
        return data[RandomUtil.randomInt(0, data.length - 1)];
    }

    /**
     * Generate current time.
     *
     * @param formats formats
     * @return time
     */
    public static String current(final String... formats) {
        String format = DEFAULT_FORMAT;
        if (Objects.nonNull(formats) && formats.length != 0 && Objects.nonNull(formats[0])) {
            format = formats[0];
        }
        LocalDateTime now = LocalDateTime.now();
        try {
            return DateTimeFormatter.ofPattern(format).format(now);
        } catch (DateTimeException e) {
            LOG.warn("format fail,use default format :{}", DEFAULT_FORMAT);
            return DateTimeFormatter.ofPattern(DEFAULT_FORMAT).format(now);
        }

    }

    /**
     * Generate array.
     *
     * @param item   item
     * @param length the length of array
     * @return array
     */
    public static Object[] array(final Object item, final int length) {
        Object[] array = new Object[length];
        Arrays.fill(array, item);
        return array;
    }

    /**
     * how to json this Object?
     * com.fasterxml.jackson.databind.ser.std.NumberSerializer#serialize
     */
    public static class FormatDouble extends Number {

        private final Double val;

        private final String format;

        public FormatDouble(final Double val, final String format) {
            this.val = val;
            this.format = format;
        }

        public FormatDouble(final Double val) {
            this.val = val;
            this.format = null;
        }

        @Override
        public String toString() {
            if (Objects.isNull(format)) {
                return val.toString();
            }
            return String.format(format, val);
        }

        @Override
        public int intValue() {
            return val.intValue();
        }

        @Override
        public long longValue() {
            return val.longValue();
        }

        @Override
        public float floatValue() {
            return val.floatValue();
        }

        @Override
        public double doubleValue() {
            return val;
        }
    }

}
