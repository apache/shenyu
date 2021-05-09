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

package org.apache.shenyu.admin.config;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.Random;

/**
 * Test SecretProperties.
 */
@Slf4j
public final class SecretPropertiesTest extends AbstractConfigurationTest {
    @Test
    public void testSecretPropertiesAssignValue() {
        long randomLong = new Random().nextLong();
        String randomKey = String.valueOf(Math.abs(randomLong));
        if (log.isDebugEnabled()) {
            log.debug("RandomKey is: " + randomKey);
        }
        load(SecretPropertiesConfiguration.class, "shenyu.aes.secret.key=" + randomKey);
        SecretProperties secretProperties = getContext().getBean(SecretProperties.class);
        Assertions.assertEquals(secretProperties.getKey(), randomKey);
    }
    
    @Configuration
    @EnableConfigurationProperties(SecretProperties.class)
    static class SecretPropertiesConfiguration {
    }
}
