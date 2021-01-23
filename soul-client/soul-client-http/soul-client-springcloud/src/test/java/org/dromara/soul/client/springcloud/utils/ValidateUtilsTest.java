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

package org.dromara.soul.client.springcloud.utils;

import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.core.env.Environment;

import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ValidateUtils}.
 *
 * @author severez
 */
@RunWith(MockitoJUnitRunner.class)
public final class ValidateUtilsTest {
    @Mock
    private Environment env;

    @Test
    public void testValidatePass() {
        SoulSpringCloudConfig soulSpringCloudConfig = new SoulSpringCloudConfig();
        soulSpringCloudConfig.setAdminUrl("http://127.0.0.1:8080");
        soulSpringCloudConfig.setContextPath("/test");
        when(env.getProperty("spring.application.name")).thenReturn("spring-cloud-test");
        ValidateUtils.validate(soulSpringCloudConfig, env);
    }

    @Test(expected = RuntimeException.class)
    public void testValidateThrowException() {
        ValidateUtils.validate(new SoulSpringCloudConfig(), env);
    }
}
