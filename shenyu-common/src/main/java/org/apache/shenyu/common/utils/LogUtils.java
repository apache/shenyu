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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.function.Supplier;

/**
 * LogUtils.
 */
public final class LogUtils {

    final static Logger logger = LoggerFactory.getLogger(findCaller().getClassName());


    /**
     * debug log.
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void debug(final String format, final Supplier<Object> supplier) {
        if (logger.isDebugEnabled()) {
            logger.debug(format, supplier.get());
        }
    }

    /**
     * debug log.
     * @param format   format
     * @param objects params
     */
    public static void debug(final String format, final Object... objects) {
        if (logger.isDebugEnabled()) {
            logger.debug(format, objects);
        }
    }

    /**
     * debug log.
     * @param supplier  {@linkplain Supplier}
     */
    public static void debug(final Supplier<Object> supplier) {
        if (logger.isDebugEnabled()) {
            logger.debug(Objects.toString(supplier.get()));
        }
    }

    /**
     * info log.
     *
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void info(final String format, final Supplier<Object> supplier) {
        if (logger.isInfoEnabled()) {
            logger.info(format, supplier.get());
        }
    }

    /**
     * info log.
     *
     * @param format   format
     * @param objects  objects
     */
    public static void info(final String format, final Object... objects) {
        if (logger.isInfoEnabled()) {
            logger.info(format, objects);
        }
    }

    /**
     * info log.
     *
     * @param supplier {@linkplain Supplier}
     */
    public static void info(final Supplier<Object> supplier) {
        if (logger.isInfoEnabled()) {
            logger.info(Objects.toString(supplier.get()));
        }
    }

    /**
     * error log.
     *
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void error(final String format, final Supplier<Object> supplier) {
        if (logger.isErrorEnabled()) {
            logger.error(format, supplier.get());
        }
    }

    /**
     * error log.
     *
     * @param format   format
     * @param objects params
     */
    public static void error(final String format, final Object... objects) {
        if (logger.isErrorEnabled()) {
            logger.error(format, objects);
        }
    }


    /**
     * error log.
     *
     * @param supplier {@linkplain Supplier}
     */
    public static void error(final Supplier<Object> supplier) {
        if (logger.isErrorEnabled()) {
            logger.error(Objects.toString(supplier.get()));
        }
    }

    /**
     * warn log.
     *
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void warn(final String format, final Supplier<Object> supplier) {
        if (logger.isWarnEnabled()) {
            logger.warn(format, supplier.get());
        }
    }

    /**
     * warn log.
     *
     * @param format   format
     * @param objects params
     */
    public static void warn(final String format, final Object... objects) {
        if (logger.isWarnEnabled()) {
            logger.warn(format, objects);
        }
    }

    /**
     * warn log.
     *
     * @param supplier {@linkplain Supplier}
     */
    public static void warn(final Supplier<Object> supplier) {
        if (logger.isWarnEnabled()) {
            logger.warn(Objects.toString(supplier.get()));
        }
    }


    /**
     * debug log.
     * @param logger   logger
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void debug(final Logger logger, final String format, final Supplier<Object> supplier) {
        if (logger.isDebugEnabled()) {
            logger.debug(format, supplier.get());
        }
    }

    /**
     * debug log.
     * @param logger   logger
     * @param supplier  {@linkplain Supplier}
     */
    public static void debug(final Logger logger, final Supplier<Object> supplier) {
        if (logger.isDebugEnabled()) {
            logger.debug(Objects.toString(supplier.get()));
        }
    }

    /**
     * info log.
     *
     * @param logger   logger
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void info(final Logger logger, final String format, final Supplier<Object> supplier) {
        if (logger.isInfoEnabled()) {
            logger.info(format, supplier.get());
        }
    }

    /**
     * info log.
     *
     * @param logger   logger
     * @param supplier {@linkplain Supplier}
     */
    public static void info(final Logger logger, final Supplier<Object> supplier) {
        if (logger.isInfoEnabled()) {
            logger.info(Objects.toString(supplier.get()));
        }
    }

    /**
     * error log.
     *
     * @param logger   logger
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void error(final Logger logger, final String format, final Supplier<Object> supplier) {
        if (logger.isErrorEnabled()) {
            logger.error(format, supplier.get());
        }
    }

    /**
     * error log.
     *
     * @param logger   logger
     * @param supplier {@linkplain Supplier}
     */
    public static void error(final Logger logger, final Supplier<Object> supplier) {
        if (logger.isErrorEnabled()) {
            logger.error(Objects.toString(supplier.get()));
        }
    }

    /**
     * warn log.
     *
     * @param logger   logger
     * @param format   format
     * @param supplier {@linkplain Supplier}
     */
    public static void warn(final Logger logger, final String format, final Supplier<Object> supplier) {
        if (logger.isWarnEnabled()) {
            logger.warn(format, supplier.get());
        }
    }

    /**
     * warn log.
     *
     * @param logger   logger
     * @param supplier {@linkplain Supplier}
     */
    public static void warn(final Logger logger, final Supplier<Object> supplier) {
        if (logger.isWarnEnabled()) {
            logger.warn(Objects.toString(supplier.get()));
        }
    }

    private static StackTraceElement findCaller() {
        StackTraceElement[] callStack = Thread.currentThread().getStackTrace();
        StackTraceElement caller;
        String logClassName = LogUtils.class.getName();
        int i = 0;
        for (int len = callStack.length; i < len; i++) {
            if (logClassName.equals(callStack[i].getClassName())) {
                break;
            }
        }
        caller = callStack[i + 2];
        return caller;
    }

}
