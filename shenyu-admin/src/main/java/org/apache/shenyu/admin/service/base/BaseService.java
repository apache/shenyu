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

package org.apache.shenyu.admin.service.base;

import java.nio.charset.StandardCharsets;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.web.client.DefaultResponseErrorHandler;
import org.springframework.web.client.RestTemplate;

/**
 * BaseService.
 */
public abstract class BaseService {

    private static RestTemplate restTemplate = new RestTemplate();

    static {
        // Solve the problem of throwing exceptions if the statuscode is not equal to 200.
        restTemplate.setErrorHandler(new DefaultResponseErrorHandler() {
            protected boolean hasErrorfinal(final HttpStatus statusCode) {
                return statusCode == null;
            }
        });
    }

    /**
     * getRestTemplate.
     *
     * @return RestTemplate
     */
    public static RestTemplate getRestTemplate() {
        // HTTP message converter.
        List<HttpMessageConverter<?>> messageConverters = restTemplate.getMessageConverters();
        messageConverters.stream().forEach(messageConverter -> {
            if (messageConverter instanceof StringHttpMessageConverter) {
                StringHttpMessageConverter stringHttpMessageConverter = (StringHttpMessageConverter) messageConverter;
                // Solve Chinese garbled code.
                stringHttpMessageConverter.setDefaultCharset(StandardCharsets.UTF_8);
            }
        });
        return restTemplate;
    }
}
