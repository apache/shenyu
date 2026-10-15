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

package org.apache.shenyu.plugin.base;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Captures log events emitted by a single logger, so that a test can assert on
 * what a code path did (or did not) log.
 *
 * <p>The appender is detached and stopped when the instance is closed, so a
 * capture never leaks into subsequent tests running in the same JVM.
 */
final class LogCapture implements AutoCloseable {

    private final Logger logger;

    private final ListAppender<ILoggingEvent> appender;

    private LogCapture(final Logger logger) {
        this.logger = logger;
        this.appender = new ListAppender<>();
        this.appender.setContext(logger.getLoggerContext());
        this.appender.start();
        logger.addAppender(this.appender);
    }

    /**
     * Attach a capture to the logger of the given type.
     *
     * @param type the class whose logger should be captured
     * @return the capture, to be used as a try-with-resources resource
     */
    static LogCapture attach(final Class<?> type) {
        Logger logger = (Logger) LoggerFactory.getLogger(type);
        return new LogCapture(logger);
    }

    /**
     * All formatted messages that were logged at INFO level during the capture.
     *
     * @return the INFO messages
     */
    List<String> infoMessages() {
        return appender.list.stream()
                .filter(event -> Level.INFO.equals(event.getLevel()))
                .map(ILoggingEvent::getFormattedMessage)
                .collect(Collectors.toList());
    }

    @Override
    public void close() {
        logger.detachAppender(appender);
        appender.stop();
    }
}
