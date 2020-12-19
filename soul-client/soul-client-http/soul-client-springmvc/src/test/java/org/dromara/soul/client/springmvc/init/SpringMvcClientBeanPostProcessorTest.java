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
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
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
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SpringMvcClientBeanPostProcessorTest {

    private final SpringMvcClientTestBeanSpecified springMvcClientTestBeanSpecified = new SpringMvcClientTestBeanSpecified();

    private final SpringMvcClientTestBeanWildcardMatching springMvcClientTestBeanWildcardMatching = new SpringMvcClientTestBeanWildcardMatching();

    private final SpringMvcClientTestBeanWithRuleName springMvcClientTestBeanWithRuleName = new SpringMvcClientTestBeanWithRuleName();

    @Test(expected = RuntimeException.class)
    public void testCreateWithContextPathIsNull() {
        createBeanPostProcessorWithContextPathIsNull();
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithContextPathIsEmpty() {
        createBeanPostProcessorWithContextPathIsEmpty();
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithAdminUrlIsNull() {
        createBeanPostProcessorWithAdminUrlIsNull();
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithAdminUrlIsEmpty() {
        createBeanPostProcessorWithAdminUrlIsEmpty();
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithPortIsNull() {
        createBeanPostProcessorWithPortIsNull();
    }

    @Test
    public void testSoulBeanProcess() {
        createBeanPostProcessor().postProcessAfterInitialization(springMvcClientTestBeanSpecified, "SpringMvcClientTestBeanSpecified");
    }

    @Test
    public void testSoulBeanProcessWithoutHost() {
        createBeanPostProcessorWithoutHost().postProcessAfterInitialization(springMvcClientTestBeanSpecified, "SpringMvcClientTestBeanSpecified");
    }

    @Test
    public void testSoulBeanProcessWithEmptyHost() {
        createBeanPostProcessorWithEmptyHost().postProcessAfterInitialization(springMvcClientTestBeanSpecified, "SpringMvcClientTestBeanSpecified");
    }

    @Test
    public void testSoulBeanProcessWithoutFull() {
        createBeanPostProcessorWithoutFull().postProcessAfterInitialization(springMvcClientTestBeanSpecified, "SpringMvcClientTestBeanSpecified");
    }

    @Test
    public void testSoulBeanProcessWildcardMatching() {
        createBeanPostProcessorWithoutFull().postProcessAfterInitialization(springMvcClientTestBeanWildcardMatching, "SpringMvcClientTestBeanWildcardMatching");
    }

    @Test
    public void testBeanProcessWithRuleName() {
        createBeanPostProcessorWithoutFull().postProcessAfterInitialization(springMvcClientTestBeanWithRuleName, "springMvcClientTestBeanWithRuleName");
    }

    @Test
    public void testNormalBeanProcess() {
        createBeanPostProcessor().postProcessAfterInitialization(new Object(), "normalBean");
    }

    private void createBeanPostProcessorWithContextPathIsNull() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setContextPath(null);
        new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private void createBeanPostProcessorWithContextPathIsEmpty() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setContextPath("");
        new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private void createBeanPostProcessorWithAdminUrlIsNull() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setAdminUrl(null);
        new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private void createBeanPostProcessorWithAdminUrlIsEmpty() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setAdminUrl("");
        new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private void createBeanPostProcessorWithPortIsNull() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setPort(null);
        new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private SpringMvcClientBeanPostProcessor createBeanPostProcessorWithoutFull() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setFull(false);
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private SpringMvcClientBeanPostProcessor createBeanPostProcessorWithoutHost() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setFull(false);
        soulSpringMvcConfig.setHost(null);
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private SpringMvcClientBeanPostProcessor createBeanPostProcessorWithEmptyHost() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        soulSpringMvcConfig.setFull(false);
        soulSpringMvcConfig.setHost("");
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private SpringMvcClientBeanPostProcessor createBeanPostProcessor() {
        SoulSpringMvcConfig soulSpringMvcConfig = createSoulSpringMvcConfigWithFillAllField();
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    private SoulSpringMvcConfig createSoulSpringMvcConfigWithFillAllField() {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl("http://127.0.0.1:58080");
        soulSpringMvcConfig.setContextPath("test");
        soulSpringMvcConfig.setAppName("test-mvc");
        soulSpringMvcConfig.setFull(true);
        soulSpringMvcConfig.setHost("127.0.0.1");
        soulSpringMvcConfig.setPort(58889);
        return soulSpringMvcConfig;
    }

    @RestController
    @RequestMapping("/order")
    @SoulSpringMvcClient(path = "/order")
    static class SpringMvcClientTestBeanSpecified {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }

    @RestController
    @RequestMapping("/goods")
    @SoulSpringMvcClient(path = "/goods/*")
    static class SpringMvcClientTestBeanWildcardMatching {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save")
        public String save(@RequestBody final String body) {
            return "" + body;
        }

        @PostMapping("/update")
        @SoulSpringMvcClient(path = "/update")
        public String update(@RequestBody final String body) {
            return "" + body;
        }
    }

    @RestController
    @RequestMapping("/transaction")
    @SoulSpringMvcClient(path = "/transaction")
    static class SpringMvcClientTestBeanWithRuleName {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save", ruleName = "test transaction save rule name")
        public String save(@RequestBody final String body) {
            return "" + body;
        }
    }
}
