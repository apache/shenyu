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
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.dubbo.rpc.service.GenericService;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.dubbo.common.cache.DubboParam;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
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
}
