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

package org.apache.shenyu.admin.security;

import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;

/**
 * Registers the security audit servlet infrastructure. The request id filter runs at
 * {@link Ordered#HIGHEST_PRECEDENCE} so the MDC request id is populated before the Shiro filter
 * (registered by shiro-spring-boot-web with order 1) and before plain {@code Filter} beans such
 * as the cluster forward filter (Spring Boot default registration order
 * {@link Ordered#LOWEST_PRECEDENCE}); audit records for requests rejected by those filters
 * therefore always carry a request id. The filter instance is only referenced by its
 * registration bean, so Spring Boot does not register it a second time.
 */
@Configuration(proxyBeanMethods = false)
public class SecurityAuditConfiguration {

    /**
     * register the request id filter with the highest precedence.
     *
     * @return filter registration bean
     */
    @Bean
    public FilterRegistrationBean<RequestIdFilter> requestIdFilterRegistration() {
        FilterRegistrationBean<RequestIdFilter> registration = new FilterRegistrationBean<>(new RequestIdFilter());
        registration.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return registration;
    }
}
