
/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

/**
 * DateUtils.
 *
 * @author xiaoyu
 */
public class DateUtils {

    /**
     * The constant LOGGER.
     */
    public static final Logger LOGGER = LoggerFactory.getLogger(DateUtils.class);

    private static final String DATE_FORMAT_DATETIME = "yyyy-MM-dd HH:mm:ss";

    /**
     * parseLocalDateTime.
     * out put format:yyyy-MM-dd HH:mm:ss
     *
     * @param str date String
     * @return yyyy -MM-dd HH:mm:ss
     * @see LocalDateTime
     */
    public static LocalDateTime parseLocalDateTime(final String str) {
        return LocalDateTime.parse(str, DateTimeFormatter.ofPattern(DATE_FORMAT_DATETIME));
    }

    /**
     * acquireMinutesBetween.
     *
     * @param start this is start date.
     * @param end   this is start date.
     * @return The number of days between start and end, if end is after start, returns a positive number, otherwise returns a negative number
     */
    public static long acquireMinutesBetween(final LocalDateTime start, final LocalDateTime end) {
        return start.until(end, ChronoUnit.MINUTES);
    }

    /**
     * Format local date time from timestamp local date time.
     *
     * @param timestamp the timestamp
     * @return the local date time
     */
    public static LocalDateTime formatLocalDateTimeFromTimestamp(final Long timestamp) {
        return LocalDateTime.ofEpochSecond(timestamp / 1000, 0, ZoneOffset.ofHours(8));
    }


}
