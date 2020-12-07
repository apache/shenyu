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

package org.dromara.soul.client.springmvc.init;

import org.dromara.soul.client.springmvc.annotation.SoulSpringMvcClient;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * SpringMvcClientBeanPostProcessorTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public final class SpringMvcClientBeanPostProcessorTest {

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

    private SpringMvcClientBeanPostProcessor springMvcClientBeanPostProcessor;

    @Before
    public void init() {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("localhost");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setPort(8080);
        springMvcClientBeanPostProcessor = new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    @Test
    public void testSoulBeanProcess() {
        springMvcClientBeanPostProcessor.postProcessAfterInitialization(springMvcClientTestBean, "springMvcClientTestBean");
    }

    @Test
    public void testNormalBeanProcess() {
        springMvcClientBeanPostProcessor.postProcessAfterInitialization(new Object(), "normalBean");
    }

    @RestController
    @RequestMapping("/order")
    @SoulSpringMvcClient(path = "/order")
    static class SpringMvcClientTestBean {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }
}