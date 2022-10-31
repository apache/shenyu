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

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * Test Cases for DateUtils.
 */
public final class DateUtilsTest {
    
    @Test
    public void testLocalDateTimeToString() {
        LocalDateTime localDateTime = LocalDateTime.of(2020, 1, 1, 23, 50, 0, 0);
        assertEquals("2020-01-01 23:50:00", DateUtils.localDateTimeToString(localDateTime));
    }
    
    @Test
    public void testLocalDateTimeToStringWithPattern() {
        LocalDateTime localDateTime = LocalDateTime.of(2020, 1, 1, 23, 50, 0, 0);
        assertEquals("2020-01-01", DateUtils.localDateTimeToString(localDateTime, "yyyy-MM-dd"));
    }
    
    @Test
    public void testAcquireMinutesBetween() {
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = start.plusMinutes(3);
        assertEquals(3, DateUtils.acquireMinutesBetween(start, end));
    }
    
    @Test
    public void testFormatLocalDateTimeFromTimestamp() {
        LocalDateTime localDateTime1 = LocalDateTime.now(ZoneOffset.ofHours(8));
        LocalDateTime localDateTime2 = DateUtils.formatLocalDateTimeFromTimestamp(ZonedDateTime.of(localDateTime1, ZoneOffset.ofHours(8)).toInstant().toEpochMilli());
        assertEquals(localDateTime1.getYear(), localDateTime2.getYear());
        assertEquals(localDateTime1.getDayOfMonth(), localDateTime2.getDayOfMonth());
        assertEquals(localDateTime1.getMonth(), localDateTime2.getMonth());
        assertEquals(localDateTime1.getHour(), localDateTime2.getHour());
        assertEquals(localDateTime1.getMinute(), localDateTime2.getMinute());
        assertEquals(localDateTime1.getSecond(), localDateTime2.getSecond());
    }
    
    @Test
    public void testFormatLocalDateTimeFromTimestampBySystemTimezone() {
        LocalDateTime localDateTime1 = LocalDateTime.now();
        LocalDateTime localDateTime2 = DateUtils.formatLocalDateTimeFromTimestampBySystemTimezone(ZonedDateTime.of(localDateTime1, ZoneId.systemDefault()).toInstant().toEpochMilli());
        assertEquals(localDateTime1.getYear(), localDateTime2.getYear());
        assertEquals(localDateTime1.getDayOfMonth(), localDateTime2.getDayOfMonth());
        assertEquals(localDateTime1.getMonth(), localDateTime2.getMonth());
        assertEquals(localDateTime1.getHour(), localDateTime2.getHour());
        assertEquals(localDateTime1.getMinute(), localDateTime2.getMinute());
        assertEquals(localDateTime1.getSecond(), localDateTime2.getSecond());
    }
    
    @Test
    public void testParseLocalDateTime() {
        LocalDateTime localDateTime1 = LocalDateTime.of(2020, 1, 1, 0, 0, 1, 0);
        LocalDateTime localDateTime2 = DateUtils.parseLocalDateTime("2020-01-01 00:00:01");
        assertEquals(localDateTime1.getYear(), localDateTime2.getYear());
        assertEquals(localDateTime1.getDayOfMonth(), localDateTime2.getDayOfMonth());
        assertEquals(localDateTime1.getMonth(), localDateTime2.getMonth());
        assertEquals(localDateTime1.getHour(), localDateTime2.getHour());
        assertEquals(localDateTime1.getMinute(), localDateTime2.getMinute());
        assertEquals(localDateTime1.getSecond(), localDateTime2.getSecond());
        LocalDateTime localDateTime = DateUtils.parseLocalDateTime("1991-08-02T08:30:11", "yyyy-MM-dd'T'HH:mm:ss");
        assertEquals(localDateTime.getYear(), 1991);
    }

    @Test
    public void testAcquireMillisBetween() {
        LocalDateTime start = LocalDateTime.now();
        LocalDateTime end = start.plusMinutes(1);
        assertEquals(60 * 1000, DateUtils.acquireMillisBetween(start, end));
    }
}
