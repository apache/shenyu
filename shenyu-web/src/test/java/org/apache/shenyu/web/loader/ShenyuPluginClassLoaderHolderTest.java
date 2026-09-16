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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link ShenyuPluginClassLoaderHolder}.
 */
public final class ShenyuPluginClassLoaderHolderTest {

    private PluginJarParser.PluginJar pluginJar;

    @BeforeEach
    public void setUp() {
        pluginJar = mock(PluginJarParser.PluginJar.class);
        when(pluginJar.getAbsolutePath()).thenReturn("testKey");
    }

    @Test
    public void getSingleton() {
        assertNotNull(ShenyuPluginClassLoaderHolder.getSingleton());
    }

    @Test
    public void createPluginClassLoader() {
        ShenyuPluginClassLoaderHolder singleton = ShenyuPluginClassLoaderHolder.getSingleton();
        ShenyuPluginClassLoader pluginClassLoader = singleton.createPluginClassLoader(pluginJar);
        assertNotNull(pluginClassLoader);
    }

    @Test
    public void removePluginClassLoader() {
        ShenyuPluginClassLoaderHolder singleton = ShenyuPluginClassLoaderHolder.getSingleton();
        singleton.removePluginClassLoader("testKey");
    }

    @Test
    public void createPluginClassLoaderAtomicallyClosesEveryDisplacedLoader() throws InterruptedException {
        int threadCount = 32;
        String jarKey = "concurrent-test-key";
        CountingBeanFactory beanFactory = new CountingBeanFactory();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBeanFactory()).thenReturn(beanFactory);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        PluginJarParser.PluginJar concurrentPluginJar = mock(PluginJarParser.PluginJar.class);
        when(concurrentPluginJar.getAbsolutePath()).thenReturn(jarKey);
        when(concurrentPluginJar.getClazzMap()).thenReturn(Collections.singletonMap("sample.Plugin", new byte[0]));
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);
        CountDownLatch ready = new CountDownLatch(threadCount);
        CountDownLatch start = new CountDownLatch(1);

        List<Runnable> tasks = Collections.nCopies(threadCount, () -> {
            ready.countDown();
            try {
                start.await();
                ShenyuPluginClassLoaderHolder.getSingleton().createPluginClassLoader(concurrentPluginJar);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
        });
        tasks.forEach(executor::submit);
        assertTrue(ready.await(5, TimeUnit.SECONDS));
        start.countDown();
        executor.shutdown();
        assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));

        assertEquals(threadCount - 1, beanFactory.destroyCount.get());
        ShenyuPluginClassLoaderHolder.getSingleton().removePluginClassLoader(jarKey);
    }

    private static final class CountingBeanFactory extends DefaultListableBeanFactory {

        private final AtomicInteger destroyCount = new AtomicInteger();

        @Override
        public boolean containsBean(final String name) {
            return true;
        }

        @Override
        public void destroySingleton(final String beanName) {
            destroyCount.incrementAndGet();
        }

        @Override
        public void removeBeanDefinition(final String beanName) {
        }
    }
}
