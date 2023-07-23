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
 * alarm service implement
 */
public class AlarmServiceImpl implements AlarmService {
    
    private static final Logger logger = LoggerFactory.getLogger(AlarmServiceImpl.class);
    
    private final RestTemplate restTemplate;
    
    private final String adminReportUrl;
    
    private final boolean enabled;
    
    public AlarmServiceImpl(RestTemplate restTemplate, String reportUrl, boolean enabled) {
        this.restTemplate = restTemplate;
        this.adminReportUrl = reportUrl;
        this.enabled = enabled;
    }
    
    @Override
    public void alarm(AlarmContent content) {
        if (!enabled) {
            return;
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<AlarmContent> request = new HttpEntity<>(content, headers);
        ResponseEntity<Void> response = restTemplate.postForEntity(adminReportUrl, request, Void.class);
        if (response.getStatusCode() == HttpStatus.OK) {
            logger.debug("send alarm content success: {}.", content);
        } else {
            logger.debug("send alarm content failed: {}.", response.getStatusCode());
        }
    }
}
