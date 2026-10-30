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

package org.apache.shenyu.web.loader;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.implementation.FixedValue;
import net.bytebuddy.implementation.StubMethod;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import static net.bytebuddy.matcher.ElementMatchers.named;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Tests extension handler handoff through the plugin loader service.
 */
public final class ShenyuLoaderServiceHandlerTest {

    private static final String PLUGIN_NAME = "handler-reload-test";

    private CommonPluginDataSubscriber subscriber;

    private ShenyuWebHandler webHandler;

    private ShenyuLoaderService service;

    private PluginData pluginData;

    @BeforeEach
    public void setUp() {
        ShenyuConfig config = new ShenyuConfig();
        config.getExtPlugin().setEnabled(false);
        subscriber = new CommonPluginDataSubscriber(List.of(), mock(ApplicationEventPublisher.class),
                new ShenyuConfig.SelectorMatchCache(), new ShenyuConfig.RuleMatchCache());
        webHandler = mock(ShenyuWebHandler.class);
        service = new ShenyuLoaderService(webHandler, subscriber, config);
        pluginData = PluginData.builder().name(PLUGIN_NAME).enabled(true).sort(1).build();
    }

    @AfterEach
    public void tearDown() {
        BaseDataCache.getInstance().removePluginData(pluginData);
    }

    @Test
    public void testReloadRoutesConfigurationToNewHandler() throws ReflectiveOperationException {
        PluginDataHandler previous = handler();
        PluginDataHandler replacement = handler();
        activate(previous);

        activate(replacement);
        subscriber.onSubscribe(pluginData);

        verify(replacement).handlerPlugin(pluginData);
        verify(previous, never()).handlerPlugin(any());
    }

    @Test
    public void testReloadWithoutHandlerRemovesPreviousHandler() throws ReflectiveOperationException {
        PluginDataHandler previous = handler();
        activate(previous);

        activate(null);
        subscriber.onSubscribe(pluginData);

        verify(previous, never()).handlerPlugin(any());
    }

    @Test
    public void testFailedPluginActivationKeepsPreviousHandler() throws ReflectiveOperationException {
        PluginDataHandler previous = handler();
        PluginDataHandler replacement = handler();
        activate(previous);
        doThrow(new IllegalStateException("activation failed")).when(webHandler).putExtPlugins(any());

        assertThrows(InvocationTargetException.class, () -> activate(replacement));
        subscriber.onSubscribe(pluginData);

        verify(previous).handlerPlugin(pluginData);
        verify(replacement, never()).handlerPlugin(any());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testExtDirectoryRemovalDetachesHandler() throws ReflectiveOperationException {
        try (GenericApplicationContext context = new GenericApplicationContext()) {
            context.refresh();
            SpringBeanUtils.getInstance().setApplicationContext(context);
            ShenyuConfig config = new ShenyuConfig();
            config.getExtPlugin().setEnabled(false);
            webHandler = new ShenyuWebHandler(List.of(), null, config);
            service = new ShenyuLoaderService(webHandler, subscriber, config);
            ShenyuPluginClassLoader removed = createLoader("Removed", PLUGIN_NAME);
            ShenyuPluginClassLoader retained = createLoader("Retained", "retained-handler-test");
            try {
                activateResults(removed.loadUploadedJarPlugins());
                activateResults(retained.loadUploadedJarPlugins());
                final ShenyuPlugin retainedPlugin = webHandler.getPlugins().stream()
                        .filter(plugin -> "retained-handler-test".equals(plugin.named())).findFirst().orElseThrow();
                Map<String, PluginDataHandler> handlers = (Map<String, PluginDataHandler>) ReflectionTestUtils.getField(subscriber, "handlerMap");
                final PluginDataHandler retainedHandler = handlers.get("retained-handler-test");
                ShenyuExtPathPluginJarLoader.ExtPluginLoadResult loadResult = mock(ShenyuExtPathPluginJarLoader.ExtPluginLoadResult.class);
                when(loadResult.getPluginJars()).thenReturn(List.of());

                try (MockedStatic<ShenyuExtPathPluginJarLoader> loader = mockStatic(ShenyuExtPathPluginJarLoader.class)) {
                    loader.when(() -> ShenyuExtPathPluginJarLoader.loadExtendPlugins(any(), any())).thenAnswer(invocation -> {
                        Consumer<ShenyuPluginClassLoader> deactivate = invocation.getArgument(1);
                        deactivate.accept(removed);
                        return loadResult;
                    });
                    service.loadExtOrUploadPlugins(null);
                }

                assertEquals(List.of(retainedPlugin), webHandler.getPlugins());
                assertFalse(handlers.containsKey(PLUGIN_NAME));
                assertSame(retainedHandler, handlers.get("retained-handler-test"));
            } finally {
                removed.close();
                retained.close();
            }
        }
    }

    private ShenyuPluginClassLoader createLoader(final String prefix, final String pluginName) {
        String pluginClass = "fixture.directory." + prefix + "Plugin";
        byte[] pluginBytes = new ByteBuddy().subclass(Object.class).name(pluginClass).implement(ShenyuPlugin.class)
                .method(named("named")).intercept(FixedValue.value(pluginName))
                .method(named("getOrder")).intercept(FixedValue.value(1))
                .method(named("execute")).intercept(StubMethod.INSTANCE).make().getBytes();
        String handlerClass = "fixture.directory." + prefix + "Handler";
        byte[] handlerBytes = new ByteBuddy().subclass(Object.class).name(handlerClass).implement(PluginDataHandler.class)
                .method(named("pluginNamed")).intercept(FixedValue.value(pluginName)).make().getBytes();
        PluginJarParser.PluginJar jar = new PluginJarParser.PluginJar();
        jar.setAbsolutePath(prefix);
        jar.setClazzMap(Map.of(pluginClass, pluginBytes, handlerClass, handlerBytes));
        return new ShenyuPluginClassLoader(jar);
    }

    private PluginDataHandler handler() {
        PluginDataHandler handler = mock(PluginDataHandler.class);
        when(handler.pluginNamed()).thenReturn(PLUGIN_NAME);
        return handler;
    }

    private void activate(final PluginDataHandler handler) throws ReflectiveOperationException {
        ShenyuPlugin plugin = mock(ShenyuPlugin.class);
        when(plugin.named()).thenReturn(PLUGIN_NAME);
        ShenyuLoaderResult pluginResult = new ShenyuLoaderResult();
        pluginResult.setShenyuPlugin(plugin);
        ShenyuLoaderResult handlerResult = new ShenyuLoaderResult();
        handlerResult.setPluginDataHandler(handler);
        activateResults(List.of(pluginResult, handlerResult));
    }

    private void activateResults(final List<ShenyuLoaderResult> results) throws ReflectiveOperationException {
        Method method = ShenyuLoaderService.class.getDeclaredMethod("loaderPlugins", List.class);
        method.setAccessible(true);
        method.invoke(service, results);
    }
}
