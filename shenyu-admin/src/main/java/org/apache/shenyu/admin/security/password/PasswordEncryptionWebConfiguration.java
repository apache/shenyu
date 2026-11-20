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

package org.apache.shenyu.admin.security.password;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.shenyu.admin.controller.PlatformController;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Prevents the legacy query-string login route from bypassing an explicitly required transport policy.
 */
@Configuration
public class PasswordEncryptionWebConfiguration implements WebMvcConfigurer {

    private final PasswordEncryptionProperties properties;

    /**
     * Create the compatibility policy adapter.
     * @param properties encryption policy
     */
    public PasswordEncryptionWebConfiguration(final PasswordEncryptionProperties properties) {
        this.properties = properties;
    }

    @Override
    public void addInterceptors(final InterceptorRegistry registry) {
        registry.addInterceptor(new HandlerInterceptor() {
            @Override
            public boolean preHandle(final HttpServletRequest request, final HttpServletResponse response, final Object handler) {
                if (properties.isRequired() && ("GET".equals(request.getMethod()) || "HEAD".equals(request.getMethod())) && handler instanceof HandlerMethod) {
                    HandlerMethod method = (HandlerMethod) handler;
                    if (PlatformController.class.isAssignableFrom(method.getBeanType()) && "loginDashboardUser".equals(method.getMethod().getName())) {
                        throw new InvalidPasswordRequestException();
                    }
                }
                return true;
            }
        });
    }
}
