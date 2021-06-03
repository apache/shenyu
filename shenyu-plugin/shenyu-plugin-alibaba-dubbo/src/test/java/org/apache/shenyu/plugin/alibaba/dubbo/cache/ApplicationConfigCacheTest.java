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

import com.alibaba.dubbo.config.RegistryConfig;
import lombok.SneakyThrows;
import org.apache.shenyu.common.config.DubboRegisterConfig;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Field;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;


/**
 * The Test Case For ApplicationConfigCache.
 */
@RunWith(MockitoJUnitRunner.class)
public final class ApplicationConfigCacheTest {

    private ApplicationConfigCache applicationConfigCache;

    @Before
    public void setUp() {
        applicationConfigCache = ApplicationConfigCache.getInstance();
    }

    @Test
    public void getInstance() {
        assertNotNull(this.applicationConfigCache);
    }

    @Test
    public void testInit() {
        DubboRegisterConfig dubboRegisterConfig = new DubboRegisterConfig();
        dubboRegisterConfig.setRegister("zookeeper://127.0.0.1:2181");
        dubboRegisterConfig.setProtocol("dubbo");
        this.applicationConfigCache.init(dubboRegisterConfig);

        RegistryConfig registryConfig = null;
        try {
            Field registryConfigField = ApplicationConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.applicationConfigCache);
            assertNotNull(config);
            registryConfig = (RegistryConfig) config;
        } catch (NoSuchFieldException | IllegalAccessException e) {
            Assert.fail();
        }

        DubboRegisterConfig dubboRegisterConfig1 = new DubboRegisterConfig();
        dubboRegisterConfig1.setRegister("zookeeper://127.0.0.2:2181");
        dubboRegisterConfig1.setProtocol("dubbo");
        this.applicationConfigCache.init(dubboRegisterConfig1);

        RegistryConfig registryConfig1 = null;
        try {
            Field registryConfigField = ApplicationConfigCache.class.getDeclaredField("registryConfig");
            registryConfigField.setAccessible(true);
            Object config = registryConfigField.get(this.applicationConfigCache);
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
        assertNotNull(this.applicationConfigCache.initRef(metaData));
    }

    @Test
    public void testGet() {
        assertNotNull(this.applicationConfigCache.get("/test"));
    }

    @Test
    public void testBuild() {
        ApplicationConfigCache.DubboParamExtInfo dubboParamExtInfo = new ApplicationConfigCache.DubboParamExtInfo();
        dubboParamExtInfo.setVersion("2.6.5");
        dubboParamExtInfo.setGroup("Group");
        dubboParamExtInfo.setUrl("http://192.168.55.113/dubbo");
        MetaData metaData = new MetaData();
        metaData.setRpcExt(GsonUtils.getInstance().toJson(dubboParamExtInfo));
        assertNotNull(this.applicationConfigCache.build(metaData));
    }

    @Test
    public void testInvalidate() {
        this.applicationConfigCache.invalidate("/test");
        this.applicationConfigCache.invalidateAll();
    }

    @SneakyThrows
    @Test
    public void testBuildLoadBalanceName() {
        assertThat(ReflectionTestUtils.invokeMethod(this.applicationConfigCache, "buildLoadBalanceName", LoadBalanceEnum.HASH.getName()), is("consistenthash"));
        assertThat(ReflectionTestUtils.invokeMethod(this.applicationConfigCache, "buildLoadBalanceName", LoadBalanceEnum.ROUND_ROBIN.getName()), is("roundrobin"));
    }
}
