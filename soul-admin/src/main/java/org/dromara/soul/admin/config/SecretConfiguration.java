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

package org.dromara.soul.admin.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Aes Secret configuration.
 *
 * @author nuo-promise
 * @author Jiang Jining
 */
@Configuration
public class SecretConfiguration {
    
    /**
     * Register secretProperties for CipherUtils in spring ioc.
     *
     * @param key the key read from property file, default value is 2095132720951327
     * @return secretProperties
     */
    @Bean
    @ConditionalOnMissingBean(value = SecretProperties.class)
    public SecretProperties secretProperties(@Value("${soul.aes.secret.key:2095132720951327}") final String key) {
        SecretProperties secretProperties = new SecretProperties();
        secretProperties.setKey(key);
        return secretProperties;
    }
}
