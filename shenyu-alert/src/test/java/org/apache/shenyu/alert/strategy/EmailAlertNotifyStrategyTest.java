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

package org.apache.shenyu.alert.strategy;

import org.apache.shenyu.common.dto.AlarmContent;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.thymeleaf.TemplateEngine;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

/**
 * Test case for EmailAlertNotifyStrategy.
 */
public class EmailAlertNotifyStrategyTest {

    private static Method buildAlertHtmlTemplateMethod;

    private static EmailAlertNotifyStrategy strategy;

    @BeforeAll
    public static void setUp() throws Exception {
        TemplateEngine mockEngine = Mockito.mock(TemplateEngine.class);
        when(mockEngine.process(eq("mailAlarm"), any(org.thymeleaf.context.IContext.class)))
                .thenReturn("<html>Rendered mailAlarm template</html>");

        strategy = new EmailAlertNotifyStrategy(mockEngine, null);
        buildAlertHtmlTemplateMethod = EmailAlertNotifyStrategy.class
                .getDeclaredMethod("buildAlertHtmlTemplate", AlarmContent.class);
        buildAlertHtmlTemplateMethod.setAccessible(true);
    }

    private String invokeBuildAlertHtmlTemplate(final AlarmContent alert) {
        try {
            return (String) buildAlertHtmlTemplateMethod.invoke(strategy, alert);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testNullDateCreatedShouldNotThrowNpe() {
        AlarmContent alert = new AlarmContent.Builder()
                .title("test title")
                .content("test content")
                .dateCreated(null)
                .build();

        assertDoesNotThrow(() -> invokeBuildAlertHtmlTemplate(alert));
    }

    @Test
    public void testValidAlertShouldNotThrow() {
        AlarmContent alert = new AlarmContent.Builder()
                .title("test title")
                .content("test content")
                .dateCreated(new Date())
                .build();

        assertDoesNotThrow(() -> invokeBuildAlertHtmlTemplate(alert));
    }

    @Test
    public void testNullContentShouldNotThrow() {
        AlarmContent alert = new AlarmContent.Builder()
                .title("test title")
                .content(null)
                .dateCreated(new Date())
                .build();

        assertDoesNotThrow(() -> invokeBuildAlertHtmlTemplate(alert));
    }
}
