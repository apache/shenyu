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

package org.apache.shenyu.plugin.cache;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.cache.cache.ResponseSingleFlight;
import org.apache.shenyu.plugin.cache.config.CacheConfig;
import org.apache.shenyu.plugin.cache.handler.CachePluginDataHandler;
import org.apache.shenyu.plugin.cache.utils.CacheUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mockito;
import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;

/**
 * Verifies cache plugin disable, removal and re-enable transitions without an external Redis server.
 */
public class CachePluginDataHandlerLifecycleTest {

    private static final String MEMORY_CONFIG = "{\"cacheType\":\"memory\",\"singleFlightEnabled\":true}";

    private final CachePluginDataHandler handler = new CachePluginDataHandler();

    private ICache failingCache;

    @BeforeEach
    public void setUp() {
        clearState();
    }

    @AfterEach
    public void tearDown() {
        clearState();
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testSameRedisConfigurationCanBeEnabledAgain(final boolean removed) {
        PluginData data = pluginData("{\"cacheType\":\"redis\",\"url\":\"127.0.0.1:63794\"}");
        handler.handlerPlugin(data);
        LettuceConnectionFactory previousFactory = connectionFactory();
        Assertions.assertTrue(previousFactory.isRunning());

        stop(data, removed);
        Assertions.assertFalse(previousFactory.isRunning());
        Assertions.assertThrows(IllegalStateException.class, ((ReactiveRedisConnectionFactory) previousFactory)::getReactiveConnection);

        data.setEnabled(true);
        handler.handlerPlugin(data);
        LettuceConnectionFactory currentFactory = connectionFactory();
        Assertions.assertNotSame(previousFactory, currentFactory);
        Assertions.assertTrue(currentFactory.isRunning());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testSameConfigurationRestoresSingleFlight(final boolean removed) {
        PluginData data = pluginData(MEMORY_CONFIG);
        handler.handlerPlugin(data);
        ResponseSingleFlight previous = CacheUtils.getSingleFlight();
        previous.lead("before-stop").orElseThrow().release();

        stop(data, removed);
        Assertions.assertTrue(CacheUtils.getSingleFlight().lead("while-disabled").isEmpty());
        data.setEnabled(true);
        handler.handlerPlugin(data);

        ResponseSingleFlight current = CacheUtils.getSingleFlight();
        Optional<ResponseSingleFlight.Fill> fill = current.lead("after-enable");
        Assertions.assertTrue(fill.isPresent());
        try {
            Mono<Optional<byte[]>> follower = current.follow("after-enable").orElseThrow();
            byte[] response = new byte[]{1, 2, 3};
            StepVerifier.create(follower)
                    .then(() -> fill.orElseThrow().complete(Optional.of(response)))
                    .assertNext(packet -> Assertions.assertArrayEquals(response, packet.orElseThrow()))
                    .expectComplete()
                    .verify(Duration.ofSeconds(5));
        } finally {
            fill.ifPresent(ResponseSingleFlight.Fill::release);
        }
    }

    @Test
    public void testRepeatedStopDetachesPublishedStateBeforeClosingOnce() {
        PluginData data = pluginData(MEMORY_CONFIG);
        handler.handlerPlugin(data);
        CacheUtils.getCache().close();
        ICache previous = Mockito.mock(ICache.class);
        Mockito.doAnswer(invocation -> {
            Assertions.assertNull(CacheUtils.getCache());
            Assertions.assertNull(Singleton.INST.get(CacheConfig.class));
            Assertions.assertTrue(CacheUtils.getSingleFlight().lead("during-close").isEmpty());
            return null;
        }).when(previous).close();
        Singleton.INST.single(ICache.class, previous);

        data.setEnabled(false);
        handler.handlerPlugin(data);
        handler.removePlugin(data);
        handler.handlerPlugin(null);

        Mockito.verify(previous, Mockito.times(1)).close();
        Assertions.assertNull(CacheUtils.getCache());
        Assertions.assertNull(Singleton.INST.get(CacheConfig.class));
    }

    @Test
    public void testCloseFailureDoesNotPreventSameConfigurationReenable() {
        PluginData data = pluginData(MEMORY_CONFIG);
        handler.handlerPlugin(data);
        CacheUtils.getCache().close();
        failingCache = Mockito.mock(ICache.class);
        IllegalStateException failure = new IllegalStateException("close failed");
        Mockito.doThrow(failure).when(failingCache).close();
        Singleton.INST.single(ICache.class, failingCache);

        data.setEnabled(false);
        Assertions.assertSame(failure, Assertions.assertThrows(IllegalStateException.class, () -> handler.handlerPlugin(data)));
        Assertions.assertNull(CacheUtils.getCache());
        Assertions.assertNull(Singleton.INST.get(CacheConfig.class));
        data.setEnabled(true);
        handler.handlerPlugin(data);

        Assertions.assertNotNull(CacheUtils.getCache());
        Assertions.assertNotSame(failingCache, CacheUtils.getCache());
        CacheUtils.getSingleFlight().lead("recovered").orElseThrow().release();
        Mockito.verify(failingCache, Mockito.times(1)).close();
    }

    @Test
    public void testActiveDuplicateUpdatePreservesCacheAndCoordinator() {
        PluginData data = pluginData(MEMORY_CONFIG);
        handler.handlerPlugin(data);
        ICache previous = CacheUtils.getCache();
        final ResponseSingleFlight coordinator = CacheUtils.getSingleFlight();
        byte[] response = new byte[]{1, 2, 3};
        previous.cacheData("cached-response", response, 60).block();

        handler.handlerPlugin(data);

        Assertions.assertSame(previous, CacheUtils.getCache());
        Assertions.assertSame(coordinator, CacheUtils.getSingleFlight());
        Assertions.assertArrayEquals(response, CacheUtils.getCache().getData("cached-response").block());
    }

    @Test
    public void testExistingFillCanCompleteAfterReenable() {
        PluginData data = pluginData(MEMORY_CONFIG);
        handler.handlerPlugin(data);
        ResponseSingleFlight previous = CacheUtils.getSingleFlight();
        ResponseSingleFlight.Fill fill = previous.lead("existing-fill").orElseThrow();
        Mono<Optional<byte[]>> follower = previous.follow("existing-fill").orElseThrow();
        byte[] response = new byte[]{1, 2, 3};

        try {
            stop(data, false);
            data.setEnabled(true);
            handler.handlerPlugin(data);

            StepVerifier.create(follower)
                    .then(() -> fill.complete(Optional.of(response)))
                    .assertNext(packet -> Assertions.assertArrayEquals(response, packet.orElseThrow()))
                    .expectComplete()
                    .verify(Duration.ofSeconds(5));
        } finally {
            fill.release();
        }
    }

    private PluginData pluginData(final String config) {
        PluginData data = new PluginData();
        data.setEnabled(true);
        data.setConfig(config);
        return data;
    }

    private void stop(final PluginData data, final boolean removed) {
        if (removed) {
            handler.removePlugin(data);
        } else {
            data.setEnabled(false);
            handler.handlerPlugin(data);
        }
    }

    private LettuceConnectionFactory connectionFactory() {
        ReactiveRedisTemplate<?, ?> template = (ReactiveRedisTemplate<?, ?>) ReflectionTestUtils.getField(CacheUtils.getCache(), "redisTemplate");
        Assertions.assertNotNull(template);
        return (LettuceConnectionFactory) template.getConnectionFactory();
    }

    private void clearState() {
        final ICache cache = CacheUtils.getCache();
        Singleton.INST.remove(ICache.class);
        Singleton.INST.remove(CacheConfig.class);
        Singleton.INST.remove(ResponseSingleFlight.class);
        ApplicationConfigCache.getInstance().invalidateAll();
        if (Objects.nonNull(cache) && cache != failingCache) {
            cache.close();
        }
    }
}
