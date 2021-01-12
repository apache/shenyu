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
 * Test case for {@link SpringMvcClientBeanPostProcessor}.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class SpringMvcClientBeanPostProcessorTest {

    private static final String ADMIN_URL = "http://127.0.0.1:58080";

    private static final String CONTEXT_PATH = "test";

    private static final boolean FULL = true;

    private static final String HOST = "127.0.0.1";

    private static final int PORT = 58889;

    private static final String EMPTY_STRING = "";

    private static final String APP_NAME = "test-mvc";

    private final TestBeanSpecified testBeanSpecified = new TestBeanSpecified();

    private final TestBeanWildcardMatching testBeanWildcardMatching = new TestBeanWildcardMatching();

    private final TestBeanWithRuleName testBeanWithRuleName = new TestBeanWithRuleName();

    @Test(expected = RuntimeException.class)
    public void testCreateWithContextPathIsNull() {
        createBeanPostProcessor(ADMIN_URL, null, FULL, HOST, PORT);
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithContextPathIsEmpty() {
        createBeanPostProcessor(ADMIN_URL, EMPTY_STRING, FULL, HOST, PORT);
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithAdminUrlIsNull() {
        createBeanPostProcessor(null, CONTEXT_PATH, FULL, HOST, PORT);
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithAdminUrlIsEmpty() {
        createBeanPostProcessor(EMPTY_STRING, CONTEXT_PATH, FULL, HOST, PORT);
    }

    @Test(expected = RuntimeException.class)
    public void testCreateWithPortIsNull() {
        createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, FULL, HOST, null);
    }

    @Test
    public void testBeanProcess() {
        createDefaultBeanPostProcessor().postProcessAfterInitialization(testBeanSpecified, "TestBeanSpecified");
    }

    @Test
    public void testBeanProcessWithoutHost() {
        SpringMvcClientBeanPostProcessor processor = createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, !FULL, null, PORT);
        processor.postProcessAfterInitialization(testBeanSpecified, "TestBeanSpecified");
    }

    @Test
    public void testBeanProcessWithEmptyHost() {
        SpringMvcClientBeanPostProcessor processor = createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, !FULL, EMPTY_STRING, PORT);
        processor.postProcessAfterInitialization(testBeanSpecified, "TestBeanSpecified");
    }

    @Test
    public void testBeanProcessWithoutFull() {
        SpringMvcClientBeanPostProcessor processor = createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, !FULL, HOST, PORT);
        processor.postProcessAfterInitialization(testBeanSpecified, "TestBeanSpecified");
    }

    @Test
    public void testBeanProcessWildcardMatching() {
        SpringMvcClientBeanPostProcessor processor = createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, !FULL, HOST, PORT);
        processor.postProcessAfterInitialization(testBeanWildcardMatching, "TestBeanWildcardMatching");
    }

    @Test
    public void testBeanProcessWithRuleName() {
        SpringMvcClientBeanPostProcessor beanPostProcessor = createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, !FULL, HOST, PORT);
        beanPostProcessor.postProcessAfterInitialization(testBeanWithRuleName, "TestBeanWithRuleName");
    }

    @Test
    public void testBeanProcessWithNormalBean() {
        createDefaultBeanPostProcessor().postProcessAfterInitialization(new Object(), "normalBean");
    }

    private SpringMvcClientBeanPostProcessor createDefaultBeanPostProcessor() {
        return createBeanPostProcessor(ADMIN_URL, CONTEXT_PATH, FULL, HOST, PORT);
    }

    private SpringMvcClientBeanPostProcessor createBeanPostProcessor(final String adminUrl,
                                                                     final String contextPath,
                                                                     final Boolean full,
                                                                     final String host,
                                                                     final Integer port) {
        SoulSpringMvcConfig soulSpringMvcConfig = new SoulSpringMvcConfig();
        soulSpringMvcConfig.setAdminUrl(adminUrl);
        soulSpringMvcConfig.setContextPath(contextPath);
        soulSpringMvcConfig.setAppName(APP_NAME);
        soulSpringMvcConfig.setFull(full);
        soulSpringMvcConfig.setHost(host);
        soulSpringMvcConfig.setPort(port);
        return new SpringMvcClientBeanPostProcessor(soulSpringMvcConfig);
    }

    @RestController
    @RequestMapping("/order")
    @SoulSpringMvcClient(path = "/order")
    static class TestBeanSpecified {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save")
        public String save(@RequestBody final String body) {
            return EMPTY_STRING + body;
        }
    }

    @RestController
    @RequestMapping("/goods")
    @SoulSpringMvcClient(path = "/goods/*")
    static class TestBeanWildcardMatching {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save")
        public String save(@RequestBody final String body) {
            return EMPTY_STRING + body;
        }

        @PostMapping("/update")
        @SoulSpringMvcClient(path = "/update")
        public String update(@RequestBody final String body) {
            return EMPTY_STRING + body;
        }
    }

    @RestController
    @RequestMapping("/transaction")
    @SoulSpringMvcClient(path = "/transaction")
    static class TestBeanWithRuleName {
        @PostMapping("/save")
        @SoulSpringMvcClient(path = "/save", ruleName = "test transaction save rule name")
        public String save(@RequestBody final String body) {
            return EMPTY_STRING + body;
        }
    }
}
