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

package org.apache.shenyu.plugin.alibaba.dubbo.cache;

import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.dubbo.common.cache.DubboParam;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


/**
 * The Test Case For AlibabaDubboConfigCache.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AlibabaDubboConfigCacheTest {

    private AlibabaDubboConfigCache alibabaDubboConfigCache;

    @BeforeEach
    public void setUp() {
        alibabaDubboConfigCache = AlibabaDubboConfigCache.getInstance();
    }

    @Test
    public void getInstance() {
        assertNotNull(this.alibabaDubboConfigCache);
    }

    @Test
    public void testInit() {
        DubboRegisterConfig dubboRegisterConfig = new DubboRegisterConfig();
        dubboRegisterConfig.setRegister("zookeeper://127.0.0.1:2181");
        dubboRegisterConfig.setProtocol("dubbo");
        this.alibabaDubboConfigCache.init(dubboRegisterConfig);

        RegistryConfig registryConfig = null;
        try {
            Field registryConfigField = AlibabaDubboConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.alibabaDubboConfigCache);
            assertNotNull(config);
            registryConfig = (RegistryConfig) config;
        } catch (NoSuchFieldException | IllegalAccessException e) {
            fail();
        }

        DubboRegisterConfig dubboRegisterConfig1 = new DubboRegisterConfig();
        dubboRegisterConfig1.setRegister("zookeeper://127.0.0.2:2181");
        dubboRegisterConfig1.setProtocol("dubbo");
        this.alibabaDubboConfigCache.init(dubboRegisterConfig1);

        RegistryConfig registryConfig1 = null;
        try {
            Field registryConfigField = AlibabaDubboConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.alibabaDubboConfigCache);
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
        AlibabaDubboConfigCache alibabaDubboConfigCacheMock = mock(AlibabaDubboConfigCache.class);
        when(alibabaDubboConfigCacheMock.initRef(metaData))
                .thenReturn(new ReferenceConfig<>());
        assertNotNull(alibabaDubboConfigCacheMock.initRef(metaData));
    }

    @Test
    public void testGet() {
        assertNotNull(this.alibabaDubboConfigCache.get("/test"));
    }

    @Test
    public void testBuild() {
        DubboParam dubboParam = new DubboParam();
        dubboParam.setVersion("2.6.5");
        dubboParam.setGroup("Group");
        dubboParam.setUrl("http://192.168.55.113/dubbo");
        MetaData metaData = new MetaData();
        metaData.setRpcExt(GsonUtils.getInstance().toJson(dubboParam));
        AlibabaDubboConfigCache alibabaDubboConfigCacheMock = mock(AlibabaDubboConfigCache.class);
        when(alibabaDubboConfigCacheMock.build(metaData))
                .thenReturn(new ReferenceConfig<>());
        assertNotNull(alibabaDubboConfigCacheMock.build(metaData));
    }

    @Test
    public void testInvalidate() {
        this.alibabaDubboConfigCache.invalidate("/test");
        this.alibabaDubboConfigCache.invalidateAll();
    }
}
