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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.AlarmContent;
import org.apache.shenyu.common.utils.UriUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

/**
 * Alarm service implement.
 */
public class AlarmServiceImpl implements AlarmService {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(AlarmServiceImpl.class);
    
    private static final String PATH = "/alert/report";
    
    private final RestTemplate restTemplate;
    
    private final List<String> adminReportUrls;
    
    private final boolean enabled;

    public AlarmServiceImpl(final RestTemplate restTemplate, final String admins, final boolean enabled) {
        this.enabled = enabled;
        this.restTemplate = restTemplate;
        adminReportUrls = new LinkedList<>();
        String scheme = System.getProperty("scheme", "http");
        String[] urls = StringUtils.split(admins, ",");
        if (Objects.nonNull(urls)) {
            for (int index = 0; index < urls.length; index++) {
                urls[index] = UriUtils.appendScheme(urls[index], scheme);
                urls[index] = urls[index] + PATH;
            }   
            adminReportUrls.addAll(Arrays.asList(urls));
        }
    }
    
    @Override
    public void alarm(final AlarmContent content) {
        if (!enabled) {
            return;
        }
        if (adminReportUrls.isEmpty()) {
            LOGGER.error("Please config shenyu.alert.admins alarm reportUrl");
            return;
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<AlarmContent> request = new HttpEntity<>(content, headers);
        boolean success = false;
        String errorMsg = "";
        for (String reportUrl : adminReportUrls) {
            if (success) {
                continue;
            }
            try {
                ResponseEntity<Void> response = restTemplate.postForEntity(reportUrl, request, Void.class);
                if (response.getStatusCode() == HttpStatus.OK) {
                    success = true;
                    LOGGER.debug("send alarm content success: {}.", content);
                } else {
                    LOGGER.debug("send alarm content failed: {}.", response.getStatusCode());
                }
            } catch (Exception e) {
                errorMsg = "send alarm content failed: " + e.getMessage();
            }
        }
        if (!success) {
            LOGGER.error(errorMsg);
        }
    }
}
