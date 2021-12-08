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

import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.dubbo.common.cache.DubboParam;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.lang.reflect.Field;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


/**
 * The Test Case For ApacheDubboConfigCache.
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApacheDubboConfigCacheTest {

    private ApacheDubboConfigCache apacheDubboConfigCache;

    @Before
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
            Assert.fail();
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
            Assert.fail();
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
        ApacheDubboConfigCache apacheDubboConfigCacheMock = mock(ApacheDubboConfigCache.class);
        when(apacheDubboConfigCacheMock.build(metaData))
                .thenReturn(new ReferenceConfig<>());
        assertNotNull(apacheDubboConfigCacheMock.build(metaData));
    }

    @Test
    public void testInvalidate() {
        this.apacheDubboConfigCache.invalidate("/test");
        this.apacheDubboConfigCache.invalidateAll();
    }
}
