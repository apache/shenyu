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

import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.apache.shenyu.common.dto.AlarmContent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mail.javamail.JavaMailSender;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;

import jakarta.mail.Session;
import jakarta.mail.internet.MimeMessage;
import java.lang.reflect.Field;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for EmailAlertNotifyStrategy.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class EmailAlertNotifyStrategyTest {

    private static final String EMAIL_FROM = "shenyu@example.com";

    private static final String EMAIL_TO = "receiver@example.com";

    private static final String RENDERED_HTML = "<html>rendered</html>";

    private static final String DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss";

    @Mock
    private JavaMailSender javaMailSender;

    @Mock
    private TemplateEngine templateEngine;

    @Mock
    private AlertReceiverDTO receiver;

    private EmailAlertNotifyStrategy strategy;

    private final AtomicReference<Context> capturedContext = new AtomicReference<>();

    @BeforeEach
    public void setUp() throws Exception {
        strategy = new EmailAlertNotifyStrategy(templateEngine, javaMailSender);
        Field emailFromUser = EmailAlertNotifyStrategy.class.getDeclaredField("emailFromUser");
        emailFromUser.setAccessible(true);
        emailFromUser.set(strategy, EMAIL_FROM);
        when(receiver.getEmail()).thenReturn(EMAIL_TO);
        when(javaMailSender.createMimeMessage()).thenReturn(new MimeMessage((Session) null));
        when(templateEngine.process(anyString(), any(Context.class))).thenAnswer(invocation -> {
            capturedContext.set(invocation.getArgument(1));
            return RENDERED_HTML;
        });
    }

    @Test
    public void testSendWithNullDateCreated() throws Exception {
        AlarmContent alert = new AlarmContent.Builder().content("test content").build();
        strategy.send(receiver, alert);
        verify(javaMailSender).send(any(MimeMessage.class));
        Context context = capturedContext.get();
        assertNotNull(context);
        String lastTriggerTime = (String) context.getVariable("lastTriggerTime");
        assertNotNull(lastTriggerTime);
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_PATTERN);
        long delta = Math.abs(new Date().getTime() - sdf.parse(lastTriggerTime).getTime());
        assertTrue(delta < 2000, "fallback time should be close to current time, but was " + lastTriggerTime);
    }

    @Test
    public void testSendWithDateCreated() throws Exception {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_TIME_PATTERN);
        Date fixedTime = sdf.parse("2026-08-21 10:00:00");
        AlarmContent alert = new AlarmContent.Builder().content("test content").dateCreated(fixedTime).build();
        strategy.send(receiver, alert);
        verify(javaMailSender).send(any(MimeMessage.class));
        Context context = capturedContext.get();
        assertNotNull(context);
        assertEquals("2026-08-21 10:00:00", context.getVariable("lastTriggerTime"));
    }
}
