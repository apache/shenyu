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
import net.bytebuddy.implementation.ExceptionMethod;
import net.bytebuddy.implementation.FixedValue;
import net.bytebuddy.implementation.StubMethod;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import static net.bytebuddy.matcher.ElementMatchers.named;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ShenyuExtensionLifecycleTest {

    @TempDir
    private Path directory;

    @Test
    @SuppressWarnings("unchecked")
    void replacesSameClassesRemovesMissingPluginsAndRollsBackFailedActivation() throws IOException {
        GenericApplicationContext context = new GenericApplicationContext();
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        ShenyuConfig config = new ShenyuConfig();
        config.getExtPlugin().setEnabled(false);
        config.getExtPlugin().setPath(directory.toString());
        ShenyuPlugin builtIn = mock(ShenyuPlugin.class);
        when(builtIn.named()).thenReturn("builtIn");
        PluginDataHandler builtInHandler = mock(PluginDataHandler.class);
        when(builtInHandler.pluginNamed()).thenReturn("builtIn");
        ShenyuWebHandler webHandler = new ShenyuWebHandler(List.of(builtIn), null, config);
        CommonPluginDataSubscriber subscriber = new CommonPluginDataSubscriber(List.of(builtInHandler), config.getSelectorMatchCache(), config.getRuleMatchCache());
        ShenyuLoaderService service = new ShenyuLoaderService(webHandler, subscriber, config);
        Map<String, PluginDataHandler> handlers = (Map<String, PluginDataHandler>) ReflectionTestUtils.getField(subscriber, "handlerMap");
        Path jar = directory.resolve("plugin.jar");
        ShenyuPluginClassLoaderHolder holder = ShenyuPluginClassLoaderHolder.getSingleton();
        try {
            writeJar(jar, "1.0", List.of("A", "B"), false);
            service.loadExtOrUploadPlugins(null);
            assertEquals(3, webHandler.getPlugins().size());
            assertEquals(3, handlers.size());
            ShenyuPlugin previous = webHandler.getPlugins().stream().filter(plugin -> "A".equals(plugin.named())).findFirst().orElseThrow();
            final PluginDataHandler previousHandler = handlers.get("A");
            ShenyuPluginClassLoader previousLoader = (ShenyuPluginClassLoader) previous.getClass().getClassLoader();
            String previousBean = previousLoader.getPluginBeanName("fixture.reload.APlugin");
            assertSame(previous, context.getBean(previousBean));

            writeJar(jar, "2.0", List.of("A"), true);
            service.loadExtOrUploadPlugins(null);
            assertTrue(holder.hasPluginClassLoader(jar.toString(), "1.0"));
            assertEquals(3, webHandler.getPlugins().size());
            assertTrue(webHandler.getPlugins().contains(previous));
            assertSame(previousHandler, handlers.get("A"));
            assertSame(previous, context.getBean(previousBean));

            writeJar(jar, "3.0", List.of("A"), false);
            service.loadExtOrUploadPlugins(null);
            assertTrue(holder.hasPluginClassLoader(jar.toString(), "3.0"));
            assertEquals(2, webHandler.getPlugins().size());
            assertFalse(webHandler.getPlugins().stream().anyMatch(plugin -> "B".equals(plugin.named())));
            assertFalse(handlers.containsKey("B"));
            assertNotSame(previousHandler, handlers.get("A"));
            ShenyuPlugin replacement = webHandler.getPlugins().stream().filter(plugin -> "A".equals(plugin.named())).findFirst().orElseThrow();
            assertNotSame(previous, replacement);
            ShenyuPluginClassLoader replacementLoader = (ShenyuPluginClassLoader) replacement.getClass().getClassLoader();
            String replacementBean = replacementLoader.getPluginBeanName("fixture.reload.APlugin");
            assertFalse(context.containsBean(previousBean));
            assertSame(replacement, context.getBean(replacementBean));

            Files.delete(jar);
            service.loadExtOrUploadPlugins(null);
            assertEquals(List.of(builtIn), webHandler.getPlugins());
            assertEquals(Map.of("builtIn", builtInHandler), handlers);
            assertFalse(context.containsBean(replacementBean));
        } finally {
            holder.removePluginClassLoader(jar.toString());
            context.close();
        }
    }

    private void writeJar(final Path path, final String version, final List<String> names, final boolean failActivation) throws IOException {
        try (OutputStream output = Files.newOutputStream(path); JarOutputStream jar = new JarOutputStream(output)) {
            jar.putNextEntry(new JarEntry("META-INF/maven/org.apache.shenyu/plugin/pom.properties"));
            Properties properties = new Properties();
            properties.setProperty("groupId", "org.apache.shenyu");
            properties.setProperty("artifactId", "plugin");
            properties.setProperty("version", version);
            properties.store(jar, null);
            jar.closeEntry();
            for (String name : names) {
                String pluginClass = "fixture.reload." + name + "Plugin";
                byte[] plugin = new ByteBuddy().subclass(Object.class).name(pluginClass).implement(ShenyuPlugin.class)
                        .method(named("named")).intercept(FixedValue.value(name))
                        .method(named("getOrder")).intercept(failActivation
                                ? ExceptionMethod.throwing(IllegalStateException.class, "activation failed") : FixedValue.value(1))
                        .method(named("execute")).intercept(StubMethod.INSTANCE).make().getBytes();
                addClass(jar, pluginClass, plugin);
                String handlerClass = "fixture.reload." + name + "Handler";
                byte[] handler = new ByteBuddy().subclass(Object.class).name(handlerClass).implement(PluginDataHandler.class)
                        .method(named("pluginNamed")).intercept(FixedValue.value(name)).make().getBytes();
                addClass(jar, handlerClass, handler);
            }
        }
    }

    private void addClass(final JarOutputStream jar, final String name, final byte[] bytes) throws IOException {
        jar.putNextEntry(new JarEntry(name.replace('.', '/') + ".class"));
        jar.write(bytes);
        jar.closeEntry();
    }
}
