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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.mapper.AlertReceiverMapper;
import org.apache.shenyu.admin.service.impl.AlertDispatchServiceImpl;
import org.apache.shenyu.alert.AlertNotifyHandler;
import org.apache.shenyu.alert.exception.AlertNoticeException;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.apache.shenyu.common.dto.AlarmContent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


/**
 * Test case for AlertDispatchServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AlertDispatchServiceTest {

    private static final byte EMAIL_TYPE = 1;

    private static final byte WEBHOOK_TYPE = 2;

    private static final byte WECHAT_TYPE = 4;

    private static final byte UNKNOWN_TYPE = 99;

    private static final String TEST_NAMESPACE_ID = "test-namespace";

    private static final String TEST_RECEIVER_ID = "test-receiver-id";

    private static final String TEST_ALERT_TITLE = "Test Alert";

    private static final String TEST_ALERT_CONTENT = "This is a test alert";

    private AlertDispatchServiceImpl alertDispatchService;

    @Mock
    private AlertNotifyHandler emailHandler;

    @Mock
    private AlertNotifyHandler webhookHandler;

    @Mock
    private AlertNotifyHandler wechatHandler;

    @Mock
    private AlertReceiverMapper alertReceiverMapper;

    @BeforeEach
    void setUp() {
        when(emailHandler.type()).thenReturn(EMAIL_TYPE);
        when(webhookHandler.type()).thenReturn(WEBHOOK_TYPE);
        when(wechatHandler.type()).thenReturn(WECHAT_TYPE);

        List<AlertNotifyHandler> handlers = Arrays.asList(emailHandler, webhookHandler, wechatHandler);
        
        alertDispatchService = new AlertDispatchServiceImpl(handlers, alertReceiverMapper);
    }

    @AfterEach
    void tearDown() {
        if (Objects.nonNull(alertDispatchService)) {
            alertDispatchService.destroy();
        }
    }

    @Test
    void testConstructorInitializesCorrectly() {

        assertNotNull(alertDispatchService);
        
        // Verify thread pool is created
        final ThreadPoolExecutor executor = getWorkerExecutor();
        assertNotNull(executor);
        assertTrue(executor.getCorePoolSize() == 3);
        assertTrue(executor.getMaximumPoolSize() == 3);
        assertFalse(executor.isShutdown());
    }

    @Test
    void testDispatchAlertSuccess() throws InterruptedException {
        final AlertReceiverDTO receiver = createTestReceiver(EMAIL_TYPE, true, false);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(receiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        final AlarmContent alarmContent = createTestAlarmContent();
        
        // Mock the handler to count down the latch when called
        when(emailHandler.type()).thenReturn(EMAIL_TYPE);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS), "Alert dispatch should complete within 5 seconds");
        
        // Verify handler was called
        Mockito.verify(emailHandler, times(1)).send(eq(receiver), eq(alarmContent));
        verify(alertReceiverMapper, times(1)).selectAll();
    }

    @Test
    void testDispatchAlertWithNullContent() throws InterruptedException {
        final AlarmContent nullContent = null;
        
        alertDispatchService.dispatchAlert(nullContent);
        
        // wait a bit to ensure task is processed
        Thread.sleep(100);
        
        // Verify no handlers are called
        verify(emailHandler, never()).send(any(), any());
        verify(webhookHandler, never()).send(any(), any());
        verify(wechatHandler, never()).send(any(), any());
        verify(alertReceiverMapper, never()).selectAll();
    }

    @Test
    void testDispatchAlertWithMultipleReceivers() throws InterruptedException {
        final AlertReceiverDTO emailReceiver = createTestReceiver(EMAIL_TYPE, true, false);
        final AlertReceiverDTO webhookReceiver = createTestReceiver(WEBHOOK_TYPE, true, false);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(emailReceiver, webhookReceiver));
        
        final CountDownLatch latch = new CountDownLatch(2);
        final AlarmContent alarmContent = createTestAlarmContent();
        
        // Mock handlers to count down the latch when called
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));
        
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(webhookHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);

        assertTrue(latch.await(5, TimeUnit.SECONDS), "Alert dispatch should complete within 5 seconds");
        
        // Verify both handlers were called
        verify(emailHandler, times(1)).send(eq(emailReceiver), eq(alarmContent));
        verify(webhookHandler, times(1)).send(eq(webhookReceiver), eq(alarmContent));
    }

    @Test
    void testDispatchAlertWithHandlerException() throws InterruptedException {
        final AlertReceiverDTO receiver = createTestReceiver(EMAIL_TYPE, true, false);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(receiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        final AlarmContent alarmContent = createTestAlarmContent();
        
        doAnswer(invocation -> {
            latch.countDown();
            throw new AlertNoticeException("Test exception");
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS), "Alert dispatch should complete within 5 seconds");
        
        verify(emailHandler, times(1)).send(eq(receiver), eq(alarmContent));
    }

    @Test
    void testClearCache() throws Exception {
        final AlertReceiverDTO receiver = createTestReceiver(EMAIL_TYPE, true, false);
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(receiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        final AlarmContent alarmContent = createTestAlarmContent();
        
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));
        
        alertDispatchService.dispatchAlert(alarmContent);
        assertTrue(latch.await(5, TimeUnit.SECONDS));
        
        final AtomicReference<List<AlertReceiverDTO>> cacheRef = getAlertReceiverReference();
        assertNotNull(cacheRef.get());
        
        alertDispatchService.clearCache();
        
        assertNull(cacheRef.get());
    }

    @Test
    void testSendNoticeMsgSuccess() {
        final AlertReceiverDTO receiver = createTestReceiver(EMAIL_TYPE, true, false);
        final AlarmContent alarmContent = createTestAlarmContent();

        final boolean result = alertDispatchService.sendNoticeMsg(receiver, alarmContent);

        assertTrue(result);
        verify(emailHandler, times(1)).send(eq(receiver), eq(alarmContent));
    }

    @Test
    void testSendNoticeMsgWithNullReceiver() {
        final AlertReceiverDTO nullReceiver = null;
        final AlarmContent alarmContent = createTestAlarmContent();

        final boolean result = alertDispatchService.sendNoticeMsg(nullReceiver, alarmContent);

        assertFalse(result);
        verify(emailHandler, never()).send(any(), any());
    }

    @Test
    void testSendNoticeMsgWithNullReceiverType() {
        final AlertReceiverDTO receiver = createTestReceiver(null, true, false);
        final AlarmContent alarmContent = createTestAlarmContent();

        final boolean result = alertDispatchService.sendNoticeMsg(receiver, alarmContent);

        assertFalse(result);
        verify(emailHandler, never()).send(any(), any());
    }

    @Test
    void testSendNoticeMsgWithUnknownType() {
        final AlertReceiverDTO receiver = createTestReceiver(UNKNOWN_TYPE, true, false);
        final AlarmContent alarmContent = createTestAlarmContent();

        final boolean result = alertDispatchService.sendNoticeMsg(receiver, alarmContent);

        assertFalse(result);
        verify(emailHandler, never()).send(any(), any());
        verify(webhookHandler, never()).send(any(), any());
        verify(wechatHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverMatchingWithMatchAll() throws InterruptedException {
        final AlarmContent alarmContent = createTestAlarmContent();
        final AlertReceiverDTO matchAllReceiver = createTestReceiver(EMAIL_TYPE, true, true);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(matchAllReceiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS));
        verify(emailHandler, times(1)).send(eq(matchAllReceiver), eq(alarmContent));
    }

    @Test
    void testReceiverMatchingWithDisabledReceiver() throws InterruptedException {
        final AlarmContent alarmContent = createTestAlarmContent();
        final AlertReceiverDTO disabledReceiver = createTestReceiver(EMAIL_TYPE, false, false);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(disabledReceiver));
        
        alertDispatchService.dispatchAlert(alarmContent);
        
        Thread.sleep(100);
        
        verify(emailHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverMatchingWithNamespaceId() throws InterruptedException {
        final AlarmContent alarmContent = createTestAlarmContent();
        alarmContent.setNamespaceId(TEST_NAMESPACE_ID);
        
        final AlertReceiverDTO matchingReceiver = createTestReceiver(EMAIL_TYPE, true, false);
        matchingReceiver.setNamespaceId(TEST_NAMESPACE_ID);
        
        final AlertReceiverDTO nonMatchingReceiver = createTestReceiver(WEBHOOK_TYPE, true, false);
        nonMatchingReceiver.setNamespaceId("different-namespace");
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(matchingReceiver, nonMatchingReceiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS));
        verify(emailHandler, times(1)).send(eq(matchingReceiver), eq(alarmContent));
        verify(webhookHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverMatchingWithLevels() throws InterruptedException {
        final byte alertLevel = 1;
        final AlarmContent alarmContent = createTestAlarmContent();
        alarmContent.setLevel(alertLevel);
        
        final AlertReceiverDTO matchingReceiver = createTestReceiver(EMAIL_TYPE, true, false);
        matchingReceiver.setLevels(Arrays.asList((byte) 0, (byte) 1, (byte) 2));
        
        final AlertReceiverDTO nonMatchingReceiver = createTestReceiver(WEBHOOK_TYPE, true, false);
        nonMatchingReceiver.setLevels(Arrays.asList((byte) 0, (byte) 2));
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(matchingReceiver, nonMatchingReceiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS));
        verify(emailHandler, times(1)).send(eq(matchingReceiver), eq(alarmContent));
        verify(webhookHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverMatchingWithLabels() throws InterruptedException {
        final Map<String, String> alertLabels = new HashMap<>();
        alertLabels.put("service", "gateway");
        alertLabels.put("env", "prod");
        
        final AlarmContent alarmContent = createTestAlarmContent();
        alarmContent.setLabels(alertLabels);
        
        final AlertReceiverDTO matchingReceiver = createTestReceiver(EMAIL_TYPE, true, false);
        final Map<String, String> matchingLabels = new HashMap<>();
        matchingLabels.put("service", "gateway");
        matchingReceiver.setLabels(matchingLabels);
        
        final AlertReceiverDTO nonMatchingReceiver = createTestReceiver(WEBHOOK_TYPE, true, false);
        final Map<String, String> nonMatchingLabels = new HashMap<>();
        nonMatchingLabels.put("service", "api");
        nonMatchingReceiver.setLabels(nonMatchingLabels);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(matchingReceiver, nonMatchingReceiver));
        
        final CountDownLatch latch = new CountDownLatch(1);
        doAnswer(invocation -> {
            latch.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        alertDispatchService.dispatchAlert(alarmContent);
        
        assertTrue(latch.await(5, TimeUnit.SECONDS));
        verify(emailHandler, times(1)).send(eq(matchingReceiver), eq(alarmContent));
        verify(webhookHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverMatchingWithMissingAlertLabels() throws InterruptedException {
        final AlarmContent alarmContent = createTestAlarmContent();
        alarmContent.setLabels(null);
        
        final AlertReceiverDTO receiverWithLabels = createTestReceiver(EMAIL_TYPE, true, false);
        final Map<String, String> requiredLabels = new HashMap<>();
        requiredLabels.put("service", "gateway");
        receiverWithLabels.setLabels(requiredLabels);

        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(receiverWithLabels));

        alertDispatchService.dispatchAlert(alarmContent);
        
        Thread.sleep(100);
        
        // Verify receiver with required labels is not matched when alert has no labels
        verify(emailHandler, never()).send(any(), any());
    }

    @Test
    void testReceiverCacheUsage() throws InterruptedException {
        final AlarmContent alarmContent = createTestAlarmContent();
        final AlertReceiverDTO receiver = createTestReceiver(EMAIL_TYPE, true, false);
        
        when(alertReceiverMapper.selectAll()).thenReturn(Arrays.asList(receiver));
        
        final CountDownLatch latch1 = new CountDownLatch(1);
        final CountDownLatch latch2 = new CountDownLatch(1);
        
        doAnswer(invocation -> {
            latch1.countDown();
            return null;
        }).doAnswer(invocation -> {
            latch2.countDown();
            return null;
        }).when(emailHandler).send(any(AlertReceiverDTO.class), any(AlarmContent.class));

        // dispatch first alert
        alertDispatchService.dispatchAlert(alarmContent);
        assertTrue(latch1.await(5, TimeUnit.SECONDS));
        
        // dispatch second alert
        alertDispatchService.dispatchAlert(alarmContent);
        assertTrue(latch2.await(5, TimeUnit.SECONDS));
        
        // verify mapper is called only once (cache is used for second call)
        verify(alertReceiverMapper, times(1)).selectAll();
        verify(emailHandler, times(2)).send(eq(receiver), eq(alarmContent));
    }

    @Test
    void testDestroy() {
        final ThreadPoolExecutor executor = getWorkerExecutor();
        assertFalse(executor.isShutdown());

        alertDispatchService.destroy();

        assertTrue(executor.isShutdown());
    }

    @Test
    void testDestroyWithNullExecutor() throws Exception {
        final Field executorField = AlertDispatchServiceImpl.class.getDeclaredField("workerExecutor");
        executorField.setAccessible(true);
        executorField.set(alertDispatchService, null);

        alertDispatchService.destroy();
    }

    private AlarmContent createTestAlarmContent() {
        return new AlarmContent.Builder()
                .title(TEST_ALERT_TITLE)
                .content(TEST_ALERT_CONTENT)
                .level((byte) 1)
                .namespaceId(TEST_NAMESPACE_ID)
                .dateCreated(new Date())
                .dateUpdated(new Date())
                .build();
    }

    private AlertReceiverDTO createTestReceiver(final Byte type, final boolean enabled, final boolean matchAll) {
        AlertReceiverDTO receiver = new AlertReceiverDTO();
        receiver.setId(TEST_RECEIVER_ID);
        receiver.setName("Test Receiver");
        receiver.setType(type);
        receiver.setEnable(enabled);
        receiver.setMatchAll(matchAll);
        receiver.setEmail("test@example.com");
        receiver.setDateCreated(new Date());
        receiver.setDateUpdated(new Date());
        return receiver;
    }

    @SuppressWarnings("unchecked")
    private AtomicReference<List<AlertReceiverDTO>> getAlertReceiverReference() {
        try {
            Field field = AlertDispatchServiceImpl.class.getDeclaredField("alertReceiverReference");
            field.setAccessible(true);
            return (AtomicReference<List<AlertReceiverDTO>>) field.get(alertDispatchService);
        } catch (Exception e) {
            throw new RuntimeException("Failed to get alertReceiverReference field", e);
        }
    }

    private ThreadPoolExecutor getWorkerExecutor() {
        try {
            Field field = AlertDispatchServiceImpl.class.getDeclaredField("workerExecutor");
            field.setAccessible(true);
            return (ThreadPoolExecutor) field.get(alertDispatchService);
        } catch (Exception e) {
            throw new RuntimeException("Failed to get workerExecutor field", e);
        }
    }
}

