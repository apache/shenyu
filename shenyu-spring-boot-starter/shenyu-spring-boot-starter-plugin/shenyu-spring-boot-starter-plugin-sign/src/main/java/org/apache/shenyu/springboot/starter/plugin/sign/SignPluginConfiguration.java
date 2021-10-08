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

package org.apache.shenyu.springboot.starter.plugin.sign;

import org.apache.shenyu.plugin.sign.api.DefaultSignProvider;
import org.apache.shenyu.plugin.sign.api.SignService;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.sign.api.SignProvider;
import org.apache.shenyu.plugin.sign.SignPlugin;
import org.apache.shenyu.plugin.sign.service.DefaultSignService;
import org.apache.shenyu.plugin.sign.subscriber.SignAuthDataSubscriber;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Sign plugin configuration.
 */
@Configuration
public class SignPluginConfiguration {
    
    /**
     * Sign service sign service.
     *
     * @return the sign service
     */
    @Bean
    @ConditionalOnMissingBean(value = SignService.class, search = SearchStrategy.ALL)
    public SignService signService() {
        return new DefaultSignService();
    }

    /**
     * Sign plugin signer.
     *
     * @return the signer
     */
    @Bean
    @ConditionalOnMissingBean(value = SignProvider.class, search = SearchStrategy.ALL)
    public SignProvider signProvider() {
        return new DefaultSignProvider();
    }
    
    /**
     * sign plugin shenyu plugin.
     *
     * @param signService the sign service
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin signPlugin(final SignService signService) {
        return new SignPlugin(signService);
    }
    
    /**
     * Sign auth data subscriber.
     *
     * @return the auth data subscriber
     */
    @Bean
    public AuthDataSubscriber signAuthDataSubscriber() {
        return new SignAuthDataSubscriber();
    }
}
