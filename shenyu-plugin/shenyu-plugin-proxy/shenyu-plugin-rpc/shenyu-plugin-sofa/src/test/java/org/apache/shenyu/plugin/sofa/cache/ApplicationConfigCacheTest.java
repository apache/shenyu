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

package org.apache.shenyu.plugin.sofa.cache;

import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.google.common.cache.LoadingCache;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.SofaRegisterConfig;
import org.apache.shenyu.common.dto.convert.selector.SofaUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * ApplicationConfigCacheTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApplicationConfigCacheTest {

    private ApplicationConfigCache cache;

    private MetaData metaData;

    @Mock
    private SelectorData selectorData;

    @BeforeEach
    void setUp() {
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath("/sofa/findAll");
        metaData.setServiceName("org.apache.shenyu.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
        metaData.setRpcType(RpcTypeEnum.SOFA.getName());
        when(selectorData.getId()).thenReturn("153153464562434");
        cache = ApplicationConfigCache.getInstance();
        cache.invalidateAll();
    }

    @Test
    void testInitRefAndGet() {
        SofaRegisterConfig config = mock(SofaRegisterConfig.class);
        when(config.getProtocol()).thenReturn("zookeeper");
        when(config.getRegister()).thenReturn("127.0.0.1:2181");
        cache.init(config);

        ConsumerConfig<com.alipay.sofa.rpc.api.GenericService> refConfig = cache.initRef(metaData);
        assertNotNull(refConfig);
        assertEquals("org.apache.shenyu.test.dubbo.api.service.DubboTestService", refConfig.getInterfaceId());

    }

    @Test
    void testInitRef2AndGet() {

        SofaRegisterConfig config = mock(SofaRegisterConfig.class);
        when(config.getProtocol()).thenReturn("zookeeper");
        when(config.getRegister()).thenReturn("127.0.0.1:2181");
        cache.init(config);

        SofaUpstream sofaUpstream = mock(SofaUpstream.class);
        when(sofaUpstream.getProtocol()).thenReturn("zookeeper");
        when(sofaUpstream.getRegister()).thenReturn("127.0.0.1:2182");
        ConsumerConfig<com.alipay.sofa.rpc.api.GenericService> refConfig = cache.initRef(selectorData.getId(), metaData, sofaUpstream);
        assertNotNull(refConfig);
        assertEquals("org.apache.shenyu.test.dubbo.api.service.DubboTestService", refConfig.getInterfaceId());
        assertEquals("127.0.0.1:2182", refConfig.getRegistry().get(0).getAddress());
    }

    @Test
    void testInvalidateMatchesWholeKeySegmentOnly() throws Exception {
        SofaUpstream sofaUpstream = mock(SofaUpstream.class);
        when(sofaUpstream.getProtocol()).thenReturn("bolt");
        when(sofaUpstream.getRegister()).thenReturn("zookeeper://127.0.0.1:2181");
        // selector id of keyA is a plain substring of the other selector id;
        // path of keyB is a plain prefix of the path of keyA
        String keyA = cache.generateUpstreamCacheKey("15123", "/sofa/findAll", sofaUpstream);
        String keyB = cache.generateUpstreamCacheKey("91512390", "/sofa/find", sofaUpstream);
        LoadingCache<String, ConsumerConfig<com.alipay.sofa.rpc.api.GenericService>> referenceCache = loadReferenceCache();
        referenceCache.invalidateAll();
        referenceCache.put(keyA, new ConsumerConfig<>());
        referenceCache.put(keyB, new ConsumerConfig<>());
        Map<String, SofaUpstream> upstreamMap = loadUpstreamMap();
        upstreamMap.clear();
        upstreamMap.put(keyA, sofaUpstream);
        upstreamMap.put(keyB, sofaUpstream);

        cache.invalidateWithSelectorId("15123");
        assertFalse(referenceCache.asMap().containsKey(keyA));
        assertTrue(referenceCache.asMap().containsKey(keyB));
        assertFalse(upstreamMap.containsKey(keyA));
        assertTrue(upstreamMap.containsKey(keyB));

        // "/sofa/find" must not invalidate the "/sofa/findAll" entry it is a prefix of
        cache.invalidateWithMetadataPath("/sofa/find");
        assertFalse(referenceCache.asMap().containsKey(keyB));
        assertFalse(upstreamMap.containsKey(keyB));
    }

    @SuppressWarnings("unchecked")
    private LoadingCache<String, ConsumerConfig<com.alipay.sofa.rpc.api.GenericService>> loadReferenceCache() throws Exception {
        Field field = ApplicationConfigCache.class.getDeclaredField("cache");
        field.setAccessible(true);
        return (LoadingCache<String, ConsumerConfig<com.alipay.sofa.rpc.api.GenericService>>) field.get(cache);
    }

    @SuppressWarnings("unchecked")
    private Map<String, SofaUpstream> loadUpstreamMap() throws Exception {
        Field field = ApplicationConfigCache.class.getDeclaredField("UPSTREAM_CACHE_MAP");
        field.setAccessible(true);
        return (Map<String, SofaUpstream>) field.get(null);
    }

}
