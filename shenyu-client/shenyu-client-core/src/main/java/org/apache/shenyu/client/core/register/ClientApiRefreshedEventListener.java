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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.registrar.ApiRegistrar;
import org.jetbrains.annotations.NotNull;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.List;
import java.util.stream.Collectors;

public final class ClientApiRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent> {
    
    private final List<ApiRegistrar> apiRegistrars;
    
    private final ApiBeansExtractor apiBeanExtractor;
    
    public ClientApiRefreshedEventListener(final List<ApiRegistrar> apiRegistrars, final ApiBeansExtractor apiBeanExtractor) {
        this.apiBeanExtractor = apiBeanExtractor;
        this.apiRegistrars = apiRegistrars;
    }
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        // Collect all types of RPC client APIs
        List<ApiBean> apiBeans = apiBeanExtractor.extract(event.getApplicationContext());
        // Register different metadata
        // Optimization point: parallel registration
        // Each registrar holds a copy of the full API information,
        // which is not complete and can be modified during the registration process
        apiRegistrars.forEach(registrar -> registrar.register(copy(apiBeans)));
    }
    
    @NotNull
    private static List<ApiBean> copy(final List<ApiBean> apiBeans) {
        return apiBeans.stream()
                .map(ApiBean::copy)
                .collect(Collectors.toList());
    }
}
