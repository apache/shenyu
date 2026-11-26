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

package org.apache.shenyu.client.mcp;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MapPropertySource;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class McpServiceEventListenerTest {

    @Test
    void testRegistersControllerToolsForEachMappedPathAndNamespace() {
        List<McpToolsRegisterDTO> events = register("/client/", Map.of("server.servlet.context-path", "/app/", "spring.mvc.servlet.path", "/api/"),
                "first;second", 8, MappedController.class, PlainController.class, UnmappedComponent.class);
        Set<List<String>> actual = new HashSet<>();
        for (McpToolsRegisterDTO event : events) {
            String path = event.getMetaDataRegisterDTO().getPath();
            String namespace = event.getNamespaceId();
            actual.add(List.of(path, namespace));
            assertEquals(namespace, event.getMetaDataRegisterDTO().getNamespaceId());
            assertEquals("mcp", event.getMetaDataRegisterDTO().getRpcType());
            assertEquals("findItem", event.getMetaDataRegisterDTO().getMethodName());
            assertEquals("lookup", event.getMetaDataRegisterDTO().getRuleName());
            JsonObject config = JsonParser.parseString(event.getMcpConfig()).getAsJsonObject();
            assertEquals("lookup", config.get("name").getAsString());
            assertRequestTemplate(config, path, "POST");
        }
        Set<List<String>> expected = new HashSet<>();
        for (String path : List.of("/items/find", "/items/search", "/catalog/find", "/catalog/search")) {
            for (String namespace : List.of("first", "second")) {
                expected.add(List.of("/client/app/api" + path, namespace));
            }
        }
        assertEquals(expected, actual);
    }

    @Test
    void testRegistersDefaultToolWithNoClassOrMethodPath() {
        McpToolsRegisterDTO event = register("/client", Map.of(), "first", 1, DefaultController.class).get(0);
        JsonObject config = JsonParser.parseString(event.getMcpConfig()).getAsJsonObject();
        assertEquals("status", config.get("name").getAsString());
        assertEquals("status", config.get("description").getAsString());
        assertEquals("/client", event.getMetaDataRegisterDTO().getPath());
        assertRequestTemplate(config, "/client", "GET");
    }

    @Test
    void testResolvesSpringMappingPlaceholdersInTheRegisteredPath() {
        McpToolsRegisterDTO event = register("/client", Map.of("demo.tool.path", "/lookup"), "first", 1, ConfiguredController.class).get(0);
        String expectedPath = "/client/catalog/lookup";
        assertEquals(expectedPath, event.getMetaDataRegisterDTO().getPath());
        JsonObject config = JsonParser.parseString(event.getMcpConfig()).getAsJsonObject();
        assertRequestTemplate(config, expectedPath, "GET");
    }

    private static void assertRequestTemplate(final JsonObject config, final String path, final String method) {
        JsonObject request = JsonParser.parseString(config.get("requestConfig").getAsString()).getAsJsonObject();
        JsonObject template = request.getAsJsonObject("requestTemplate");
        assertEquals(path, template.get("url").getAsString());
        assertEquals(method, template.get("method").getAsString());
    }

    private static List<McpToolsRegisterDTO> register(final String contextPath, final Map<String, Object> env,
                                                      final String namespaces, final int registrations, final Class<?>... controllers) {
        Properties properties = new Properties();
        properties.setProperty("appName", "demo");
        properties.setProperty("contextPath", contextPath);
        properties.setProperty("host", "127.0.0.1");
        properties.setProperty("port", "8080");
        ShenyuClientConfig.ClientPropertiesConfig mcp = new ShenyuClientConfig.ClientPropertiesConfig();
        mcp.setProps(properties);
        ShenyuClientConfig config = new ShenyuClientConfig();
        config.setClient(Map.of("mcp", mcp));
        config.setNamespace(namespaces);
        ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
        ArgumentCaptor<McpToolsRegisterDTO> captor = ArgumentCaptor.forClass(McpToolsRegisterDTO.class);
        try (MockedStatic<ShenyuClientRegisterEventPublisher> singleton = mockStatic(ShenyuClientRegisterEventPublisher.class);
             AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            singleton.when(ShenyuClientRegisterEventPublisher::getInstance).thenReturn(publisher);
            context.getEnvironment().getPropertySources().addFirst(new MapPropertySource("test", env));
            context.addApplicationListener(new McpServiceEventListener(config, mock(ShenyuClientRegisterRepository.class), context.getEnvironment()));
            context.register(controllers);
            context.refresh();
            verify(publisher, times(registrations)).publishEvent(captor.capture());
            return captor.getAllValues();
        }
    }

    @Controller
    @ShenyuMcpTool
    @RequestMapping({"/items/", "/catalog"})
    static class MappedController {

        @PostMapping({"/find", "/search"})
        @ShenyuMcpTool(toolName = "lookup")
        public String findItem() {
            return "found";
        }

        @GetMapping("/ignored")
        public String ignored() {
            return "ignored";
        }
    }

    @Controller
    @RequestMapping("/plain")
    static class PlainController {

        @GetMapping("/ignored")
        @ShenyuMcpTool
        public String ignored() {
            return "ignored";
        }
    }

    @Component
    @ShenyuMcpTool
    @RequestMapping("/component")
    static class UnmappedComponent {

        @GetMapping("/ignored")
        @ShenyuMcpTool
        public String ignored() {
            return "ignored";
        }
    }

    @Controller
    @ShenyuMcpTool
    static class DefaultController {

        @GetMapping
        @ShenyuMcpTool
        public String status() {
            return "ok";
        }
    }

    @Controller
    @ShenyuMcpTool
    @RequestMapping("${demo.catalog.path:/catalog}/")
    static class ConfiguredController {

        @GetMapping("${demo.tool.path}")
        @ShenyuMcpTool
        public String lookup() {
            return "found";
        }
    }
}
