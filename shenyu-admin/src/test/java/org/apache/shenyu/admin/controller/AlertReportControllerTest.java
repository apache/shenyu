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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.service.AlertDispatchService;
import org.apache.shenyu.common.dto.AlarmContent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for AlertReportController.
 */
@ExtendWith(MockitoExtension.class)
public class AlertReportControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private AlertReportController alertReportController;

    @Mock
    private AlertDispatchService alertDispatchService;

    @BeforeEach
    public void setUp() {
        LocalValidatorFactoryBean validator = new LocalValidatorFactoryBean();
        validator.afterPropertiesSet();
        this.mockMvc = MockMvcBuilders.standaloneSetup(alertReportController)
                .setValidator(validator)
                .build();
    }

    @Test
    public void testBlankTitleReturns400() throws Exception {
        String body = "{\"title\":\"\",\"content\":\"test content\",\"level\":1}";

        this.mockMvc.perform(post("/alert/report")
                .contentType(MediaType.APPLICATION_JSON)
                .content(body))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void testNullTitleReturns400() throws Exception {
        String body = "{\"content\":\"test content\",\"level\":1}";

        this.mockMvc.perform(post("/alert/report")
                .contentType(MediaType.APPLICATION_JSON)
                .content(body))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void testBlankContentReturns400() throws Exception {
        String body = "{\"title\":\"test title\",\"content\":\"   \",\"level\":1}";

        this.mockMvc.perform(post("/alert/report")
                .contentType(MediaType.APPLICATION_JSON)
                .content(body))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void testNullContentReturns400() throws Exception {
        String body = "{\"title\":\"test title\",\"level\":1}";

        this.mockMvc.perform(post("/alert/report")
                .contentType(MediaType.APPLICATION_JSON)
                .content(body))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void testValidRequestDispatchesCorrectly() throws Exception {
        String body = "{\"title\":\"test title\",\"content\":\"test content\",\"level\":1,\"namespaceId\":\"ns-1\",\"labels\":{\"key\":\"value\"}}";

        this.mockMvc.perform(post("/alert/report")
                .contentType(MediaType.APPLICATION_JSON)
                .content(body))
                .andExpect(status().isOk());

        ArgumentCaptor<AlarmContent> captor = ArgumentCaptor.forClass(AlarmContent.class);
        verify(alertDispatchService).dispatchAlert(captor.capture());
        AlarmContent dispatched = captor.getValue();
        assertEquals("test title", dispatched.getTitle());
        assertEquals("test content", dispatched.getContent());
        assertEquals((byte) 1, dispatched.getLevel());
        assertEquals("ns-1", dispatched.getNamespaceId());
        assertEquals("value", dispatched.getLabels().get("key"));
    }
}
