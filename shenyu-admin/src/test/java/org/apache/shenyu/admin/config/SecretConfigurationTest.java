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

import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test SecretConfiguration.
 */
public class SecretConfigurationTest extends AbstractConfigurationTest {
    
    @Test
    public void testNormalSecretConfiguration() {
        load(SecretConfiguration.class, "shenyu.aes.secret.key=1234567890123456");
        SecretProperties secretProperties = getContext().getBean(SecretProperties.class);
        Assertions.assertNotNull(secretProperties);
        String key = secretProperties.getKey();
        Assertions.assertEquals(key, "1234567890123456");
    }
    
    @Test
    public void testDefaultSecretConfiguration() {
        load(SecretConfiguration.class);
        SecretProperties secretProperties = getContext().getBean(SecretProperties.class);
        Assertions.assertNotNull(secretProperties);
        String key = secretProperties.getKey();
        Assertions.assertEquals(key, "2095132720951327");
    }
    
    @Test
    public void testAbnormalSecretConfiguration() {
        load(SecretConfiguration.class, "shenyu.aes.secret.key=");
        SecretProperties secretProperties = getContext().getBean(SecretProperties.class);
        Assertions.assertNotNull(secretProperties);
        String key = secretProperties.getKey();
        Assertions.assertEquals("", key);
    }
}
