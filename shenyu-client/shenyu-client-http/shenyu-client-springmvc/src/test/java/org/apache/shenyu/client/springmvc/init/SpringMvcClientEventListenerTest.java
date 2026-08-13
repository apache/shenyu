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

package org.apache.shenyu.client.springmvc.init;

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.javatuples.Sextet;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig.ClientPropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link SpringMvcClientEventListener}.
 */
@ExtendWith(MockitoExtension.class)
public class SpringMvcClientEventListenerTest {

    private final MockedStatic<RegisterUtils> registerUtilsMockedStatic = mockStatic(RegisterUtils.class);

    private final SpringMvcClientTestBean springMvcClientTestBean = new SpringMvcClientTestBean();

    private final SpringMvcClientTestBean2 springMvcClientTestBean2 = new SpringMvcClientTestBean2();

    private final SpringMvcClientTestBean3 springMvcClientTestBean3 = new SpringMvcClientTestBean3();

    private final SpringMvcClientTestBean4 springMvcClientTestBean4 = new SpringMvcClientTestBean4();

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private AutowireCapableBeanFactory beanFactory;

    @Mock
    private Environment env;

    private ContextRefreshedEvent contextRefreshedEvent;

    private void init() {
        Map<String, Object> results = new LinkedHashMap<>();
        results.put("springMvcClientTestBean", springMvcClientTestBean);
        results.put("springMvcClientTestBean2", springMvcClientTestBean2);
        results.put("springMvcClientTestBean3", springMvcClientTestBean3);
        results.put("springMvcClientTestBean4", springMvcClientTestBean4);
        when(applicationContext.getBeansWithAnnotation(any())).thenReturn(results);
        when(applicationContext.getEnvironment()).thenReturn(env);
        when(env.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE)).thenReturn("local");
        contextRefreshedEvent = new ContextRefreshedEvent(applicationContext);
        ShenyuClientConfig shenyuClientConfig = mock(ShenyuClientConfig.class);
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new SpringMvcClientEventListener(shenyuClientConfig, mock(ShenyuClientRegisterRepository.class), env));
    }

    @Test
    public void testShenyuBeanProcess() {
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        // config with full
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(true, true);
        springMvcClientEventListener.onApplicationEvent(new ContextRefreshedEvent(applicationContext));
        verify(applicationContext, never()).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testNormalBeanProcess() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, true);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testWithShenyuClientAnnotation() {
        init();
        registerUtilsMockedStatic.when(() -> RegisterUtils.doLogin(any(), any(), any())).thenReturn(Optional.of("token"));
        registerUtilsMockedStatic.when(() -> RegisterUtils.doRegister(any(), any(), any()))
                .thenAnswer((Answer<Void>) invocation -> null);
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, true);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        verify(applicationContext, times(2)).getBeansWithAnnotation(any());
        registerUtilsMockedStatic.close();
    }

    private SpringMvcClientEventListener buildSpringMvcClientEventListener(final boolean full, final boolean port) {
        Properties properties = new Properties();
        properties.setProperty("contextPath", "/mvc");
        properties.setProperty("isFull", String.valueOf(full));
        properties.setProperty("ip", "127.0.0.1");
        if (port) {
            properties.setProperty("port", "8289");
        }
        properties.setProperty("username", "admin");
        properties.setProperty("password", "123456");
        properties.setProperty(ShenyuClientConstants.DISCOVERY_LOCAL_MODE_KEY, Boolean.TRUE.toString());
        ShenyuClientConfig.ClientPropertiesConfig config = new ShenyuClientConfig.ClientPropertiesConfig();
        config.setProps(properties);
        ShenyuRegisterCenterConfig mockRegisterCenter = new ShenyuRegisterCenterConfig();
        mockRegisterCenter.setServerLists("http://127.0.0.1:9095");
        mockRegisterCenter.setRegisterType("http");
        mockRegisterCenter.setProps(properties);
        ShenyuClientConfig shenyuClientConfig = new ShenyuClientConfig();
        Map<String, ClientPropertiesConfig> client = new HashMap<>();
        client.put("http", config);
        shenyuClientConfig.setClient(client);
        return new SpringMvcClientEventListener(shenyuClientConfig, ShenyuClientRegisterRepositoryFactory.newInstance(mockRegisterCenter), env);
    }

    @Test
    public void testOnApplicationEvent() {
        init();
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, false);
        when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);
        MockedStatic<PortUtils> portUtilsMockedStatic = mockStatic(PortUtils.class);
        portUtilsMockedStatic.when(() -> PortUtils.findPort(beanFactory)).thenReturn(8080);
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);

        // hit `!registered.compareAndSet(false, true)`
        springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent);
        portUtilsMockedStatic.close();
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testOnApplicationEventError() {
        init();
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, false);
        Assert.assertThrows(ShenyuException.class, () -> springMvcClientEventListener.onApplicationEvent(contextRefreshedEvent));
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testOnApplicationEventFullModeShouldRegisterOnce() {
        try (MockedStatic<ShenyuClientRegisterEventPublisher> publisherMockedStatic = mockStatic(ShenyuClientRegisterEventPublisher.class)) {
            ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
            publisherMockedStatic.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
            SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(true, true);
            ContextRefreshedEvent event = new ContextRefreshedEvent(applicationContext);
            springMvcClientEventListener.onApplicationEvent(event);
            springMvcClientEventListener.onApplicationEvent(event);
            verify(publisher, times(1)).start(any());
            verify(publisher, times(2)).publishEvent(any());
        }
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testOnBuildApiSuperPath() {
        SpringMvcClientEventListener springMvcClientEventListener = buildSpringMvcClientEventListener(false, false);

        Assertions.assertEquals("/order", springMvcClientEventListener.buildApiSuperPath(
                SpringMvcClientTestBean.class, AnnotatedElementUtils.findMergedAnnotation(SpringMvcClientTestBean.class, ShenyuSpringMvcClient.class)), "super-path");

        when(env.getProperty("spring.mvc.servlet.path")).thenReturn("/servlet-path");
        when(env.getProperty("server.servlet.context-path")).thenReturn("/servlet-context-path");
        Assertions.assertEquals("/servlet-context-path/servlet-path/order", springMvcClientEventListener.buildApiSuperPath(
                SpringMvcClientTestBean.class, AnnotatedElementUtils.findMergedAnnotation(SpringMvcClientTestBean.class, ShenyuSpringMvcClient.class)), "super-path");
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testBuildApiSuperPathsMultiPath() {
        SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
        List<String> paths = listener.buildApiSuperPaths(
                SpringMvcMultiPathTestBean.class,
                AnnotatedElementUtils.findMergedAnnotation(SpringMvcMultiPathTestBean.class, ShenyuSpringMvcClient.class));
        Assertions.assertEquals(2, paths.size());
        Assertions.assertTrue(paths.stream().anyMatch(p -> p.endsWith("/multi-a")));
        Assertions.assertTrue(paths.stream().anyMatch(p -> p.endsWith("/multi-b")));
        registerUtilsMockedStatic.close();
    }

    @Test
    public void testBuildApiPathMethodAnnotationTakesPrecedence() throws Exception {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = SpringMvcClientTestBean.class.getDeclaredMethod("hello", String.class);
            ShenyuSpringMvcClient methodAnnotation = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            // Method has path="/hello", so it should be used regardless of superPath
            String apiPath = listener.buildApiPath(method, "/order", methodAnnotation);
            Assertions.assertEquals("/mvc/order/hello", apiPath);
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiPathEmptyMethodAnnotationFallsThroughToRequestMapping() throws Exception {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = SpringMvcClientTestBean.class.getDeclaredMethod("hello2", String.class);
            ShenyuSpringMvcClient methodAnnotation = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            // Method has path="" (empty), so it should fall through to @GetMapping("/hello2")
            String apiPath = listener.buildApiPath(method, "/order", methodAnnotation);
            Assertions.assertEquals("/mvc/order/hello2", apiPath);
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiPathSuffixOverlapDoesNotCauseFalseMatch() throws Exception {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            // @ShenyuSpringMvcClient(path = "/order") on method, superPath = "/prefix/order"
            // The old endsWith heuristic would falsely skip the annotation path here
            Method method = SpringMvcSuffixOverlapTestBean.class.getDeclaredMethod("greet");
            ShenyuSpringMvcClient methodAnnotation = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            String apiPath = listener.buildApiPath(method, "/prefix/order", methodAnnotation);
            Assertions.assertEquals("/mvc/prefix/order/order", apiPath);
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiPathClassLevelFallbackUsesRequestMapping() throws Exception {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = SpringMvcClientTestBean.class.getDeclaredMethod("hello3", String.class);
            // Method has no @ShenyuSpringMvcClient → class-level annotation path="/order" should be skipped
            // @GetMapping("") → should just return superPath
            String apiPath = listener.buildApiPathFromRequestMapping(method, "/order");
            Assertions.assertEquals("/mvc/order", apiPath);
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiPathMultiPathMethodAnnotation() throws Exception {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = SpringMvcMultiPathApiDocTestBean.class.getDeclaredMethod("greet");
            ShenyuSpringMvcClient methodAnnotation = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            // Method has path="/greet" with class multi-path {"/multi-a", "/multi-b"}
            // superPath="/multi-a" should NOT suppress "/greet" (different path)
            String apiPath = listener.buildApiPath(method, "/multi-a", methodAnnotation);
            Assertions.assertEquals("/mvc/multi-a/greet", apiPath);
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testMultiPathControllerPublishesCorrectMetaDataAndApiDoc() {
        try {
            SpringMvcMultiPathApiDocTestBean multiPathBean = new SpringMvcMultiPathApiDocTestBean();

            Map<String, Object> controllerBeans = new LinkedHashMap<>();
            controllerBeans.put("multiPathApiDocBean", multiPathBean);
            when(applicationContext.getBeansWithAnnotation(eq(Controller.class))).thenReturn(controllerBeans);

            Map<String, Object> apiModuleBeans = new LinkedHashMap<>();
            apiModuleBeans.put("multiPathApiDocBean", multiPathBean);
            when(applicationContext.getBeansWithAnnotation(eq(ApiModule.class))).thenReturn(apiModuleBeans);

            when(applicationContext.getEnvironment()).thenReturn(env);
            when(env.getProperty("shenyu.discovery.type", ShenyuClientConstants.DISCOVERY_LOCAL_MODE)).thenReturn("local");
            when(applicationContext.getAutowireCapableBeanFactory()).thenReturn(beanFactory);

            try (MockedStatic<ShenyuClientRegisterEventPublisher> publisherMock = mockStatic(ShenyuClientRegisterEventPublisher.class);
                 MockedStatic<PortUtils> portUtilsMock = mockStatic(PortUtils.class)) {

                ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
                publisherMock.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
                portUtilsMock.when(() -> PortUtils.findPort(beanFactory)).thenReturn(8080);

                SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
                ContextRefreshedEvent event = new ContextRefreshedEvent(applicationContext);
                listener.onApplicationEvent(event);

                ArgumentCaptor<DataTypeParent> captor = ArgumentCaptor.forClass(DataTypeParent.class);
                verify(publisher, atLeastOnce()).publishEvent(captor.capture());

                List<DataTypeParent> events = captor.getAllValues();

                // Verify MetaDataRegisterDTO: 2 paths × 1 method = 2 entries
                List<MetaDataRegisterDTO> metaDatas = events.stream()
                        .filter(e -> e instanceof MetaDataRegisterDTO)
                        .map(e -> (MetaDataRegisterDTO) e)
                        .collect(Collectors.toList());
                Assertions.assertEquals(2, metaDatas.size(),
                        "Expected 2 metadata entries for 2 class-level paths × 1 method");
                List<String> metaPaths = metaDatas.stream()
                        .map(MetaDataRegisterDTO::getPath)
                        .sorted()
                        .collect(Collectors.toList());
                Assertions.assertTrue(metaPaths.get(0).endsWith("/multi-a/greet"),
                        "Expected path ending with /multi-a/greet but got: " + metaPaths.get(0));
                Assertions.assertTrue(metaPaths.get(1).endsWith("/multi-b/greet"),
                        "Expected path ending with /multi-b/greet but got: " + metaPaths.get(1));

                // Verify ApiDocRegisterDTO: 2 paths × 1 path-value × 1 HTTP method = 2 entries
                List<ApiDocRegisterDTO> apiDocs = events.stream()
                        .filter(e -> e instanceof ApiDocRegisterDTO)
                        .map(e -> (ApiDocRegisterDTO) e)
                        .collect(Collectors.toList());
                Assertions.assertEquals(2, apiDocs.size(),
                        "Expected 2 API doc entries for 2 class-level paths × 1 method");
                List<String> apiPaths = apiDocs.stream()
                        .map(ApiDocRegisterDTO::getApiPath)
                        .sorted()
                        .collect(Collectors.toList());
                Assertions.assertTrue(apiPaths.get(0).endsWith("/multi-a/greet"),
                        "Expected apiPath ending with /multi-a/greet but got: " + apiPaths.get(0));
                Assertions.assertTrue(apiPaths.get(1).endsWith("/multi-b/greet"),
                        "Expected apiPath ending with /multi-b/greet but got: " + apiPaths.get(1));
            }
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiDocSextetDefaultProducesConsumes() throws NoSuchMethodException {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = ApiDocTestBean.class.getDeclaredMethod("getDefault");
            Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> result =
                    listener.buildApiDocSextet(method, null, Collections.emptyMap());

            Assertions.assertArrayEquals(new String[]{"/get-default"}, result.getValue0());
            Assertions.assertEquals("*/*", result.getValue1());
            Assertions.assertEquals("*/*", result.getValue2());
            Assertions.assertArrayEquals(new ApiHttpMethodEnum[]{ApiHttpMethodEnum.GET}, result.getValue3());
            Assertions.assertEquals(RpcTypeEnum.HTTP, result.getValue4());
            Assertions.assertEquals("v0.01", result.getValue5());
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiDocSextetExplicitProducesConsumesAndMethod() throws NoSuchMethodException {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = ApiDocTestBean.class.getDeclaredMethod("postExplicit", String.class);
            Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> result =
                    listener.buildApiDocSextet(method, null, Collections.emptyMap());

            Assertions.assertArrayEquals(new String[]{"/post-explicit"}, result.getValue0());
            Assertions.assertEquals("application/json", result.getValue1());
            Assertions.assertEquals("application/json", result.getValue2());
            Assertions.assertArrayEquals(new ApiHttpMethodEnum[]{ApiHttpMethodEnum.POST}, result.getValue3());
            Assertions.assertEquals(RpcTypeEnum.HTTP, result.getValue4());
            Assertions.assertEquals("v0.01", result.getValue5());
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @Test
    public void testBuildApiDocSextetMultipleMethodsProducesConsumes() throws NoSuchMethodException {
        try {
            SpringMvcClientEventListener listener = buildSpringMvcClientEventListener(false, false);
            Method method = ApiDocTestBean.class.getDeclaredMethod("multi", String.class);
            Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> result =
                    listener.buildApiDocSextet(method, null, Collections.emptyMap());

            Assertions.assertArrayEquals(new String[]{"/multi"}, result.getValue0());
            Assertions.assertEquals("application/json,application/xml", result.getValue1());
            Assertions.assertEquals("application/json,application/xml", result.getValue2());
            List<ApiHttpMethodEnum> methods = Arrays.asList(result.getValue3());
            Assertions.assertTrue(methods.contains(ApiHttpMethodEnum.GET));
            Assertions.assertTrue(methods.contains(ApiHttpMethodEnum.POST));
            Assertions.assertEquals(2, methods.size());
            Assertions.assertEquals(RpcTypeEnum.HTTP, result.getValue4());
            Assertions.assertEquals("v0.01", result.getValue5());
        } finally {
            registerUtilsMockedStatic.close();
        }
    }

    @RestController
    @RequestMapping("/order")
    @ShenyuSpringMvcClient(path = "/order")
    static class SpringMvcClientTestBean {

        @GetMapping("/hello")
        @ShenyuSpringMvcClient(path = "/hello")
        public String hello(@RequestBody final String input) {
            return "hello:" + input;
        }

        @GetMapping("/hello2")
        @ShenyuSpringMvcClient(path = "")
        public String hello2(@RequestBody final String input) {
            return "hello:" + input;
        }

        @GetMapping("")
        public String hello3(@RequestBody final String input) {
            return "hello:" + input;
        }
    }

    @RestController
    @RequestMapping("/hello2/*")
    static class SpringMvcClientTestBean2 {
        public String test(final String hello) {
            return hello;
        }
    }

    @RestController
    static class SpringMvcClientTestBean3 {
        public String test(final String hello) {
            return hello;
        }
    }

    @RestController
    @ShenyuSpringMvcClient(path = "/order/*")
    static class SpringMvcClientTestBean4 {
        public String test(final String hello) {
            return hello;
        }
    }

    @RestController
    @ShenyuSpringMvcClient(path = {"/multi-a", "/multi-b"})
    static class SpringMvcMultiPathTestBean {
        public String test() {
            return "ok";
        }
    }

    @RestController
    @RequestMapping({"/multi-a", "/multi-b"})
    @ShenyuSpringMvcClient(path = {"/multi-a", "/multi-b"})
    @ApiModule(value = "multiPathApiDoc")
    static class SpringMvcMultiPathApiDocTestBean {

        @GetMapping("/greet")
        @ShenyuSpringMvcClient(path = "/greet")
        @ApiDoc(desc = "greet")
        public String greet() {
            return "hello from multipath";
        }
    }

    @RestController
    @ShenyuSpringMvcClient(path = "/prefix/order")
    static class SpringMvcSuffixOverlapTestBean {

        @GetMapping("/test")
        @ShenyuSpringMvcClient(path = "/order")
        public String greet() {
            return "ok";
        }
    }

    @RestController
    static class ApiDocTestBean {

        @GetMapping(value = "/get-default")
        public String getDefault() {
            return "ok";
        }

        @RequestMapping(value = "/post-explicit",
                method = RequestMethod.POST,
                produces = "application/json",
                consumes = "application/json")
        public String postExplicit(@RequestBody final String input) {
            return input;
        }

        @RequestMapping(value = "/multi",
                method = {RequestMethod.GET, RequestMethod.POST},
                produces = {"application/json", "application/xml"},
                consumes = {"application/json", "application/xml"})
        public String multi(@RequestBody final String input) {
            return input;
        }
    }

}
