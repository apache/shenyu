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

package org.apache.shenyu.web.configuration;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link SpringExtConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class SpringExtConfigurationTest {

    private SpringExtConfiguration springExtConfiguration;

    @BeforeEach
    public void before() {
        springExtConfiguration = new SpringExtConfiguration();
    }

    @Test
    public void applicationContextAwareTest() {
        ApplicationContextAware applicationContextAware = springExtConfiguration.applicationContextAware();
        ConfigurableApplicationContext applicationContext = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        when(SpringBeanUtils.getInstance().getBean(ShenyuConfig.class))
                .thenReturn(new ShenyuConfig());
        applicationContextAware.setApplicationContext(applicationContext);
        assertNotNull(applicationContextAware);
    }

}
