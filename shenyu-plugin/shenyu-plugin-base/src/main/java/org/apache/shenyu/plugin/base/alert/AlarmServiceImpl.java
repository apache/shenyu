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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

/**
 * Alarm service implement.
 */
public class AlarmServiceImpl implements AlarmService {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(AlarmServiceImpl.class);
    
    private final RestTemplate restTemplate;
    
    private final String adminReportUrl;
    
    private final boolean enabled;
    
    public AlarmServiceImpl(final RestTemplate restTemplate, final String reportUrl, final boolean enabled) {
        this.restTemplate = restTemplate;
        this.adminReportUrl = reportUrl;
        this.enabled = enabled;
    }
    
    @Override
    public void alarm(final AlarmContent content) {
        if (!enabled) {
            return;
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<AlarmContent> request = new HttpEntity<>(content, headers);
        ResponseEntity<Void> response = restTemplate.postForEntity(adminReportUrl, request, Void.class);
        if (response.getStatusCode() == HttpStatus.OK) {
            LOGGER.debug("send alarm content success: {}.", content);
        } else {
            LOGGER.debug("send alarm content failed: {}.", response.getStatusCode());
        }
    }
}
