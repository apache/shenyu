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

package org.apache.shenyu.plugin.apache.dubbo.cache;

import com.google.common.cache.LoadingCache;
import org.apache.dubbo.common.URL;
import org.apache.dubbo.config.ConsumerConfig;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.dubbo.rpc.service.GenericService;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.dubbo.common.cache.DubboParam;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


/**
 * The Test Case For ApacheDubboConfigCache.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ApacheDubboConfigCacheTest {

    private ApacheDubboConfigCache apacheDubboConfigCache;

    @BeforeEach
    public void setUp() {
        apacheDubboConfigCache = ApacheDubboConfigCache.getInstance();
    }

    @Test
    public void getInstance() {
        assertNotNull(this.apacheDubboConfigCache);
    }

    @Test
    public void testInit() {
        DubboRegisterConfig dubboRegisterConfig = new DubboRegisterConfig();
        dubboRegisterConfig.setRegister("zookeeper://127.0.0.1:2181");
        dubboRegisterConfig.setProtocol("dubbo");
        this.apacheDubboConfigCache.init(dubboRegisterConfig);

        RegistryConfig registryConfig = null;
        try {
            Field registryConfigField = ApacheDubboConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.apacheDubboConfigCache);
            assertNotNull(config);
            registryConfig = (RegistryConfig) config;
        } catch (NoSuchFieldException | IllegalAccessException e) {
            fail();
        }

        DubboRegisterConfig dubboRegisterConfig1 = new DubboRegisterConfig();
        dubboRegisterConfig1.setRegister("zookeeper://127.0.0.2:2181");
        dubboRegisterConfig1.setProtocol("dubbo");
        this.apacheDubboConfigCache.init(dubboRegisterConfig1);

        RegistryConfig registryConfig1 = null;
        try {
            Field registryConfigField = ApacheDubboConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.apacheDubboConfigCache);
            assertNotNull(config);
            registryConfig1 = (RegistryConfig) config;
        } catch (NoSuchFieldException | IllegalAccessException e) {
            fail();
        }
        assertNotSame(registryConfig, registryConfig1);
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "test-namespace"})
    public void testInitAppliesGroupOnlyChanges(final String namespace) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        DubboRegisterConfig config = createRegisterConfig("fixed", 4, 32, 64);
        config.setGroup("first-group");
        configCache.init(config);
        ReferenceConfig<GenericService> first = createReference(configCache, namespace);

        config.setGroup("second-group");
        configCache.init(config);
        ReferenceConfig<GenericService> second = createReference(configCache, namespace);

        assertEquals("first-group", first.getRegistry().getGroup());
        assertEquals("second-group", second.getRegistry().getGroup());
        assertEquals(config.getRegister(), ((RegistryConfig) ReflectionTestUtils.getField(configCache, "registryConfig")).getAddress());
        assertSame(first.getConsumer(), second.getConsumer());
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "test-namespace"})
    public void testInitClearsRegistryGroup(final String namespace) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        DubboRegisterConfig config = createRegisterConfig("fixed", 4, 32, 64);
        config.setGroup("first-group");
        configCache.init(config);
        RegistryConfig first = (RegistryConfig) ReflectionTestUtils.getField(configCache, "registryConfig");

        config.setGroup(null);
        configCache.init(config);

        assertEquals("first-group", first.getGroup());
        assertNull(((RegistryConfig) ReflectionTestUtils.getField(configCache, "registryConfig")).getGroup());
        assertNull(createReference(configCache, namespace).getRegistry().getGroup());
    }

    @ParameterizedTest
    @CsvSource({"cached, 4, 32, 64", "fixed, 8, 32, 64", "fixed, 4, 64, 64", "fixed, 4, 32, 128"})
    public void testInitUpdatesEachConsumerPoolField(final String threadpool, final Integer corethreads, final Integer threads, final Integer queues) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        configCache.init(createRegisterConfig("fixed", 4, 32, 64));
        final ReferenceConfig<GenericService> first = createReference(configCache, "");

        configCache.init(createRegisterConfig(threadpool, corethreads, threads, queues));
        ReferenceConfig<GenericService> second = createReference(configCache, "");

        assertEquals(threadpool, second.getConsumer().getThreadpool());
        assertEquals(corethreads, second.getConsumer().getCorethreads());
        assertEquals(threads, second.getConsumer().getThreads());
        assertEquals(queues, second.getConsumer().getQueues());
        assertEquals("fixed", first.getConsumer().getThreadpool());
        assertEquals(4, first.getConsumer().getCorethreads());
        assertEquals(32, first.getConsumer().getThreads());
        assertEquals(64, first.getConsumer().getQueues());
        assertSame(first.getRegistry(), second.getRegistry());
    }

    @ParameterizedTest
    @CsvSource({", 4, 32, 64", "fixed, , 32, 64", "fixed, 4, , 64", "fixed, 4, 32, ", ", , , "})
    public void testInitRestoresDefaultsForClearedConsumerFields(final String threadpool, final Integer corethreads, final Integer threads, final Integer queues) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        configCache.init(createRegisterConfig("fixed", 4, 32, 64));
        ConsumerConfig defaults = new ConsumerConfig();
        defaults.refresh();

        configCache.init(createRegisterConfig(threadpool, corethreads, threads, queues));
        ConsumerConfig actual = createReference(configCache, "").getConsumer();

        assertEquals(Objects.isNull(threadpool) ? defaults.getThreadpool() : threadpool, actual.getThreadpool());
        assertEquals(Objects.isNull(corethreads) ? defaults.getCorethreads() : corethreads, actual.getCorethreads());
        assertEquals(Objects.isNull(threads) ? defaults.getThreads() : threads, actual.getThreads());
        assertEquals(Objects.isNull(queues) ? defaults.getQueues() : queues, actual.getQueues());
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    public void testInitReusesUnchangedConfiguration(final boolean explicitPoolConfig) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        DubboRegisterConfig config = explicitPoolConfig ? createRegisterConfig("fixed", 4, 32, 64) : createRegisterConfig(null, null, null, null);
        config.setGroup("same-group");
        configCache.init(config);
        ReferenceConfig<GenericService> first = createReference(configCache, "");

        configCache.init(GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(config), DubboRegisterConfig.class));
        ReferenceConfig<GenericService> second = createReference(configCache, "");

        assertSame(first.getRegistry(), second.getRegistry());
        assertSame(first.getConsumer(), second.getConsumer());
    }

    @Test
    public void testInitRestoresConfiguredDubboDefault() {
        final String property = "dubbo.consumer.threads";
        final String previous = System.getProperty(property);
        try {
            System.setProperty(property, "19");
            ConsumerConfig defaults = new ConsumerConfig();
            defaults.refresh();
            assertEquals(19, defaults.getThreads());
            ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
            configCache.init(createRegisterConfig("fixed", 4, 32, 64));

            configCache.init(createRegisterConfig("fixed", 4, null, 64));

            assertEquals(19, createReference(configCache, "").getConsumer().getThreads());
        } finally {
            if (Objects.isNull(previous)) {
                System.clearProperty(property);
            } else {
                System.setProperty(property, previous);
            }
        }
    }

    private DubboRegisterConfig createRegisterConfig(final String threadpool, final Integer corethreads, final Integer threads, final Integer queues) {
        DubboRegisterConfig config = new DubboRegisterConfig();
        config.setProtocol("zookeeper");
        config.setRegister("zookeeper://127.0.0.1:2181");
        config.setThreadpool(threadpool);
        config.setCorethreads(corethreads);
        config.setThreads(threads);
        config.setQueues(queues);
        return config;
    }

    private ReferenceConfig<GenericService> createReference(final ApacheDubboConfigCache configCache, final String namespace) {
        MetaData metaData = new MetaData();
        metaData.setServiceName("org.apache.shenyu.test.DemoService");
        return ReflectionTestUtils.invokeMethod(configCache, "buildReference", metaData, namespace);
    }

    @Test
    public void testInitRef() {
        MetaData metaData = new MetaData();
        metaData.setPath("/test");
        ApacheDubboConfigCache apacheDubboConfigCacheMock = mock(ApacheDubboConfigCache.class);
        when(apacheDubboConfigCacheMock.initRef(metaData))
                .thenReturn(new org.apache.dubbo.config.ReferenceConfig<>());
        assertNotNull(apacheDubboConfigCacheMock.initRef(metaData));
    }

    @Test
    public void testGet() {
        assertNotNull(this.apacheDubboConfigCache.get("/test"));
    }

    @Test
    public void testBuild() {
        DubboParam dubboParamExtInfo = new DubboParam();
        dubboParamExtInfo.setVersion("2.7.5");
        dubboParamExtInfo.setGroup("Group");
        dubboParamExtInfo.setLoadbalance("Balance");
        dubboParamExtInfo.setUrl("http://192.168.55.113/dubbo");
        MetaData metaData = new MetaData();
        metaData.setRpcExt(GsonUtils.getInstance().toJson(dubboParamExtInfo));
        ApacheDubboConfigCache apacheDubboConfigCacheMock = new ApacheDubboConfigCache();
        DubboRegisterConfig dubboRegisterConfig = new DubboRegisterConfig();
        dubboRegisterConfig.setRegister("zookeeper://127.0.0.1:2181");
        dubboRegisterConfig.setProtocol("dubbo");
        apacheDubboConfigCacheMock.init(dubboRegisterConfig);
        assertNotNull(apacheDubboConfigCacheMock.build(metaData, ""));
    }

    @ParameterizedTest
    @CsvSource({
        "zookeeper://127.0.0.1:2181?namespace=old, zookeeper://127.0.0.1:2181?namespace=new",
        "zookeeper://127.0.0.1:2181, zookeeper://127.0.0.1:2181?namespace=new",
        "zookeeper://127.0.0.1:2181?group=g, zookeeper://127.0.0.1:2181?group=g&namespace=new",
        "zookeeper://127.0.0.1:2181?namespace=old&group=g&timeout=5000, zookeeper://127.0.0.1:2181?namespace=new&group=g&timeout=5000",
        "zookeeper://127.0.0.1:2181?group=g&namespace=old&timeout=5000, zookeeper://127.0.0.1:2181?group=g&namespace=new&timeout=5000",
        "zookeeper://my-namespace-svc:2181, zookeeper://my-namespace-svc:2181?namespace=new",
        "zookeeper://127.0.0.1:2181/namespace?group=namespace, zookeeper://127.0.0.1:2181/namespace?group=namespace&namespace=new",
        "zookeeper://user:password@127.0.0.1:2181?group=g%26x, zookeeper://user:password@127.0.0.1:2181?group=g%26x&namespace=new"
    })
    public void testBuildReferenceWithNamespace(final String address, final String expectedAddress) {
        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        RegistryConfig originalRegistry = new RegistryConfig();
        originalRegistry.setAddress(address);
        ReflectionTestUtils.setField(configCache, "registryConfig", originalRegistry);
        MetaData metaData = new MetaData();
        metaData.setServiceName("org.apache.shenyu.test.DemoService");

        ReferenceConfig<GenericService> reference = ReflectionTestUtils.invokeMethod(configCache, "buildReference", metaData, "new");

        assertNotNull(reference);
        assertRegistryAddress(expectedAddress, reference.getRegistry().getAddress());
        assertNotSame(originalRegistry, reference.getRegistry());
        assertEquals(address, originalRegistry.getAddress());
        assertSame(originalRegistry, ReflectionTestUtils.getField(configCache, "registryConfig"));
        assertFalse(reference.getRegistry().isRegister());
    }

    @Test
    public void testBuildUpstreamReferenceWithNamespace() {
        DubboUpstream upstream = DubboUpstream.builder().protocol("zookeeper").build();
        String address = "zookeeper://127.0.0.1:2181?namespace=old&group=g&timeout=5000";
        upstream.setRegistry(address);
        MetaData metaData = new MetaData();
        metaData.setServiceName("org.apache.shenyu.test.DemoService");
        RuleData ruleData = new RuleData();
        ruleData.setId("namespace-rewrite-rule");

        ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        ReferenceConfig<GenericService> reference = ReflectionTestUtils.invokeMethod(configCache, "buildReference", metaData, ruleData, "new", upstream);

        assertNotNull(reference);
        assertRegistryAddress("zookeeper://127.0.0.1:2181?namespace=new&group=g&timeout=5000", reference.getRegistry().getAddress());
        assertEquals(address, upstream.getRegistry());
        assertFalse(reference.getRegistry().isRegister());
    }

    private void assertRegistryAddress(final String expectedAddress, final String actualAddress) {
        URL expected = URL.valueOf(expectedAddress);
        URL actual = URL.valueOf(actualAddress);
        assertEquals(expected.getProtocol(), actual.getProtocol());
        assertEquals(expected.getHost(), actual.getHost());
        assertEquals(expected.getPort(), actual.getPort());
        assertEquals(expected.getPath(), actual.getPath());
        assertEquals(expected.getUsername(), actual.getUsername());
        assertEquals(expected.getPassword(), actual.getPassword());
        assertEquals(expected.getParameters(), actual.getParameters());
    }

    @Test
    public void testInvalidate() {
        this.apacheDubboConfigCache.invalidate("/test");
        this.apacheDubboConfigCache.invalidateAll();
    }

    @Test
    public void testInvalidateMatchesWholeKeySegmentOnly() throws Exception {
        DubboUpstream dubboUpstream = DubboUpstream.builder().protocol("dubbo").build();
        dubboUpstream.setRegistry("zookeeper://127.0.0.1:2181");
        dubboUpstream.setVersion("1.0.0");
        dubboUpstream.setGroup("g1");
        // selector id of keyA is a plain substring of the rule id and of the other selector id
        String keyA = apacheDubboConfigCache.generateUpstreamCacheKey("15123", "9001", "8001", "ns1", dubboUpstream);
        String keyB = apacheDubboConfigCache.generateUpstreamCacheKey("91512390", "9151239", "8002", "ns1", dubboUpstream);
        String keyC = apacheDubboConfigCache.generateUpstreamCacheKey("70000", "7001", "8003", "ns1", dubboUpstream);
        // blank namespace: the selector id becomes the first key segment
        String keyD = apacheDubboConfigCache.generateUpstreamCacheKey("15123", "9002", "8004", "", dubboUpstream);
        String pathBasedKey = "ns1:/some/path";
        LoadingCache<String, ReferenceConfig<GenericService>> cache = loadReferenceCache();
        cache.invalidateAll();
        cache.put(keyA, new ReferenceConfig<>());
        cache.put(keyB, new ReferenceConfig<>());
        cache.put(keyC, new ReferenceConfig<>());
        cache.put(keyD, new ReferenceConfig<>());
        cache.put(pathBasedKey, new ReferenceConfig<>());

        apacheDubboConfigCache.invalidateWithSelectorId("15123");
        assertFalse(cache.asMap().containsKey(keyA));
        assertFalse(cache.asMap().containsKey(keyD));
        assertTrue(cache.asMap().containsKey(keyB));
        assertTrue(cache.asMap().containsKey(keyC));
        assertTrue(cache.asMap().containsKey(pathBasedKey));

        apacheDubboConfigCache.invalidateWithRuleId("9151239");
        assertFalse(cache.asMap().containsKey(keyB));

        apacheDubboConfigCache.invalidateWithMetadataId("8003");
        assertFalse(cache.asMap().containsKey(keyC));
        assertTrue(cache.asMap().containsKey(pathBasedKey));
    }

    @SuppressWarnings("unchecked")
    private LoadingCache<String, ReferenceConfig<GenericService>> loadReferenceCache() throws Exception {
        Field field = ApacheDubboConfigCache.class.getDeclaredField("cache");
        field.setAccessible(true);
        return (LoadingCache<String, ReferenceConfig<GenericService>>) field.get(apacheDubboConfigCache);
    }

    @Test
    public void testGetOrParseUpstreamsFiltersUnusableEntries() {
        DubboUpstream active = DubboUpstream.builder().protocol("dubbo").status(true).build();
        active.setRegistry("dubbo://127.0.0.1:20880");
        DubboUpstream disabled = DubboUpstream.builder().protocol("dubbo").status(false).build();
        disabled.setRegistry("dubbo://127.0.0.1:20881");
        String handle = GsonUtils.getInstance().toJson(List.of(active, disabled));
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-1");
        selectorData.setHandle(handle);

        List<DubboUpstream> result = apacheDubboConfigCache.getOrParseUpstreams(selectorData);

        assertEquals(1, result.size());
        assertTrue(result.get(0).isStatus());
    }

    @Test
    public void testGetOrParseUpstreamsReusesParseWhileHandleUnchanged() {
        DubboUpstream active = DubboUpstream.builder().protocol("dubbo").status(true).build();
        active.setRegistry("dubbo://127.0.0.1:20880");
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-2");
        selectorData.setHandle(GsonUtils.getInstance().toJson(List.of(active)));

        List<DubboUpstream> first = apacheDubboConfigCache.getOrParseUpstreams(selectorData);
        List<DubboUpstream> second = apacheDubboConfigCache.getOrParseUpstreams(selectorData);

        assertSame(first, second);
    }

    @Test
    public void testGetOrParseUpstreamsReparsesWhenHandleChanges() {
        DubboUpstream first = DubboUpstream.builder().protocol("dubbo").status(true).build();
        first.setRegistry("dubbo://127.0.0.1:20880");
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-3");
        selectorData.setHandle(GsonUtils.getInstance().toJson(List.of(first)));
        assertEquals("dubbo://127.0.0.1:20880", apacheDubboConfigCache.getOrParseUpstreams(selectorData).get(0).getRegistry());

        DubboUpstream second = DubboUpstream.builder().protocol("dubbo").status(true).build();
        second.setRegistry("dubbo://127.0.0.2:20880");
        selectorData.setHandle(GsonUtils.getInstance().toJson(List.of(second)));
        List<DubboUpstream> reparsed = apacheDubboConfigCache.getOrParseUpstreams(selectorData);

        assertEquals(1, reparsed.size());
        assertEquals("dubbo://127.0.0.2:20880", reparsed.get(0).getRegistry());
    }

    @Test
    public void testGetOrParseUpstreamsBlankHandleReturnsEmpty() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId("selector-4");
        selectorData.setHandle(null);

        assertTrue(apacheDubboConfigCache.getOrParseUpstreams(selectorData).isEmpty());
        assertTrue(apacheDubboConfigCache.getOrParseUpstreams(null).isEmpty());
        assertNotNull(apacheDubboConfigCache.getOrParseUpstreams(selectorData));
    }
}
