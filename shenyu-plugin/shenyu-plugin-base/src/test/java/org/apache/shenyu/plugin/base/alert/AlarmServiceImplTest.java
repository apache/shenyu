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

package org.apache.shenyu.plugin.base.alert;

import org.apache.shenyu.common.dto.AlarmContent;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link AlarmServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class AlarmServiceImplTest {
    @Mock
    private RestTemplate restTemplate;

    @Test
    public void testDisabledAndMissingAdmins() {
        AlarmContent content = new AlarmContent.Builder().build();
        new AlarmServiceImpl(restTemplate, "localhost:9095", false).alarm(content);
        new AlarmServiceImpl(restTemplate, "", true).alarm(content);
        verifyNoPost();
    }

    @Test
    public void testAlarmStopsAfterSuccessfulEndpoint() {
        AlarmContent content = new AlarmContent.Builder().build();
        when(restTemplate.postForEntity(contains("localhost:9095/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenReturn(new ResponseEntity<>(HttpStatus.OK));
        new AlarmServiceImpl(restTemplate, "localhost:9095,localhost:9096", true).alarm(content);
        verify(restTemplate).postForEntity(contains("localhost:9095/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class));
        verify(restTemplate, never()).postForEntity(contains("localhost:9096/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class));
    }

    @Test
    public void testAlarmContinuesOnFailureAndException() {
        AlarmContent content = new AlarmContent.Builder().build();
        when(restTemplate.postForEntity(contains("localhost:9095/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenReturn(new ResponseEntity<>(HttpStatus.BAD_GATEWAY));
        when(restTemplate.postForEntity(contains("localhost:9096/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenThrow(new RuntimeException("down"));
        new AlarmServiceImpl(restTemplate, "localhost:9095,localhost:9096", true).alarm(content);
        verify(restTemplate, org.mockito.Mockito.times(2)).postForEntity(org.mockito.ArgumentMatchers.anyString(),
                org.mockito.ArgumentMatchers.any(), eq(Void.class));
    }

    @Test
    public void testAlarmSucceedsOnFallbackEndpoint() {
        AlarmContent content = new AlarmContent.Builder().build();
        when(restTemplate.postForEntity(contains("localhost:9095/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenReturn(new ResponseEntity<>(HttpStatus.BAD_GATEWAY));
        when(restTemplate.postForEntity(contains("localhost:9096/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenReturn(new ResponseEntity<>(HttpStatus.OK));
        new AlarmServiceImpl(restTemplate, "localhost:9095,localhost:9096,localhost:9097", true).alarm(content);
        verify(restTemplate, org.mockito.Mockito.times(2)).postForEntity(org.mockito.ArgumentMatchers.anyString(),
                org.mockito.ArgumentMatchers.any(), eq(Void.class));
        verify(restTemplate, never()).postForEntity(contains("localhost:9097/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class));
    }

    @Test
    public void testAlarmRecoversAfterEndpointThrowsException() {
        AlarmContent content = new AlarmContent.Builder().build();
        when(restTemplate.postForEntity(contains("localhost:9095/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenThrow(new RuntimeException("unreachable"));
        when(restTemplate.postForEntity(contains("localhost:9096/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class)))
                .thenReturn(new ResponseEntity<>(HttpStatus.OK));
        new AlarmServiceImpl(restTemplate, "localhost:9095,localhost:9096,localhost:9097", true).alarm(content);
        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(restTemplate).postForEntity(contains("localhost:9096/alert/report"), captor.capture(), eq(Void.class));
        HttpEntity<?> request = (HttpEntity<?>) captor.getValue();
        assertSame(content, request.getBody());
        assertEquals(MediaType.APPLICATION_JSON, request.getHeaders().getContentType());
        verify(restTemplate, never()).postForEntity(contains("localhost:9097/alert/report"), org.mockito.ArgumentMatchers.any(), eq(Void.class));
    }

    private void verifyNoPost() {
        verify(restTemplate, never()).postForEntity(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.any(), eq(Void.class));
    }
}
