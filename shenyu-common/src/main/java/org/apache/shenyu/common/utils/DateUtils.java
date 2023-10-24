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

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

/**
 * DateUtils.
 */
public final class DateUtils {
    
    public static final String DATE_FORMAT_DATETIME = "yyyy-MM-dd HH:mm:ss";

    public static final String DATE_FORMAT_DATETIME_MILLISECOND = "yyyy-MM-dd HH:mm:ss.SSS";

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DATE_FORMAT_DATETIME);
    
    private DateUtils() {
    }
    
    /**
     * parse LocalDateTime.
     * out put format: yyyy-MM-dd HH:mm:ss
     *
     * @param dataTime date String
     * @return LocalDateTime: yyyy-MM-dd HH:mm:ss
     * @see LocalDateTime
     */
    public static LocalDateTime parseLocalDateTime(final String dataTime) {
        return LocalDateTime.parse(dataTime, DateTimeFormatter.ofPattern(DATE_FORMAT_DATETIME));
    }

    /**
     * Parse local date time local date time.
     *
     * @param dataTime          the data time
     * @param dateTimeFormatter the date time formatter
     * @return the local date time
     */
    public static LocalDateTime parseLocalDateTime(final String dataTime, final String dateTimeFormatter) {
        return LocalDateTime.parse(dataTime, DateTimeFormatter.ofPattern(dateTimeFormatter));
    }
    
    /**
     * acquireMinutesBetween.
     *
     * @param start this is start date.
     * @param end   this is start date.
     * @return The number of days between start and end, if end is after start,
     *     returns a positive number, otherwise returns a negative number.
     */
    public static long acquireMinutesBetween(final LocalDateTime start, final LocalDateTime end) {
        return start.until(end, ChronoUnit.MINUTES);
    }
    
    /**
     * Acquire millis between long.
     *
     * @param start the start
     * @param end   the end
     * @return the long
     */
    public static long acquireMillisBetween(final LocalDateTime start, final LocalDateTime end) {
        return start.until(end, ChronoUnit.MILLIS);
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
    
    /**
     * Format local date time from timestamp by system time zone.
     *
     * @param timestamp the timestamp
     * @return the local date time
     */
    public static LocalDateTime formatLocalDateTimeFromTimestampBySystemTimezone(final Long timestamp) {
        return LocalDateTime.ofEpochSecond(timestamp / 1000, 0, OffsetDateTime.now().getOffset());
    }
    
    /**
     * Format local date time to string.
     * use default pattern yyyy-MM-dd HH:mm:ss
     *
     * @param localDateTime the localDateTime
     * @return the format string
     */
    public static String localDateTimeToString(final LocalDateTime localDateTime) {
        return DATE_TIME_FORMATTER.format(localDateTime);
    }
    
    /**
     * Format local date time to string.
     *
     * @param localDateTime the localDateTime
     * @param pattern       formatter pattern
     * @return the format string
     */
    public static String localDateTimeToString(final LocalDateTime localDateTime, final String pattern) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);
        return localDateTime.format(formatter);
    }
}
