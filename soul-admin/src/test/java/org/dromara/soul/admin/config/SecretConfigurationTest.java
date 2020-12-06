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

import org.dromara.soul.admin.AbstractConfigurationTest;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * Test SecretConfiguration.
 *
 * @author Jiang Jining
 */
public class SecretConfigurationTest extends AbstractConfigurationTest {
    
    @Test
    public void testNormalSecretConfiguration() {
        load(SecretConfiguration.class, "soul.aes.secret.key=2095132720951327");
        SecretProperties secretProperties = getContext().getBean(SecretProperties.class);
        String key = secretProperties.getKey();
        Assertions.assertNull(key);
    }
    
    @Test
    public void testAbnormalSecretConfiguration() {
        load(SecretConfiguration.class, "soul.aes.secret.key=soul.aes.secret.key=1095132720951327");
        AnnotationConfigApplicationContext context = getContext();
        Assertions.assertThrows(NoSuchBeanDefinitionException.class, () -> context.getBean(SecretProperties.class));
    }
}
