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

package org.apache.shenyu.common.dto;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test case for AlarmContent.
 */
public class AlarmContentTest {

    @Test
    public void testBuilderAndGetterSetter() {
        AlarmContent alarmContent = new AlarmContent.Builder()
                .title("test title")
                .content("test content")
                .level((byte) 1)
                .build();

        assertTrue("test title".equals(alarmContent.getTitle()));
        assertTrue("test content".equals(alarmContent.getContent()));
        assertTrue(alarmContent.getLevel() == 1);

        alarmContent.setTitle("new title");
        alarmContent.setContent("new content");
        alarmContent.setLevel((byte) 2);

        assertTrue("new title".equals(alarmContent.getTitle()));
        assertTrue("new content".equals(alarmContent.getContent()));
        assertTrue(alarmContent.getLevel() == 2);
    }
}
