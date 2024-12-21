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

package org.apache.shenyu.client.apache.dubbo.annotation;

import org.apache.dubbo.config.annotation.DubboService;
import org.apache.dubbo.config.annotation.Service;
import org.junit.Assert;
import org.junit.Test;

public class ShenyuDubboServiceDefaultValueTest {

    @Test
    public void dubboServiceDefaultValue() {
        ShenyuDubboService shenyuDubboService = ShenyuDubboServiceTest.class.getAnnotation(ShenyuDubboService.class);
        DubboService dubboService = DubboServiceTest.class.getAnnotation(DubboService.class);

        Assert.assertEquals(shenyuDubboService.interfaceClass(), dubboService.interfaceClass());
        Assert.assertEquals(shenyuDubboService.interfaceName(), dubboService.interfaceName());
        Assert.assertEquals(shenyuDubboService.version(), dubboService.version());
        Assert.assertEquals(shenyuDubboService.group(), dubboService.group());
        Assert.assertEquals(shenyuDubboService.dubboPath(), dubboService.path());
        Assert.assertEquals(shenyuDubboService.export(), dubboService.export());
        Assert.assertEquals(shenyuDubboService.token(), dubboService.token());
        Assert.assertEquals(shenyuDubboService.deprecated(), dubboService.deprecated());
        Assert.assertEquals(shenyuDubboService.dynamic(), dubboService.dynamic());
        Assert.assertEquals(shenyuDubboService.accesslog(), dubboService.accesslog());
        Assert.assertEquals(shenyuDubboService.executes(), dubboService.executes());
        Assert.assertEquals(shenyuDubboService.register(), dubboService.register());
        Assert.assertEquals(shenyuDubboService.weight(), dubboService.weight());
        Assert.assertEquals(shenyuDubboService.document(), dubboService.document());
        Assert.assertEquals(shenyuDubboService.delay(), dubboService.delay());
        Assert.assertEquals(shenyuDubboService.local(), dubboService.local());
        Assert.assertEquals(shenyuDubboService.stub(), dubboService.stub());
        Assert.assertEquals(shenyuDubboService.cluster(), dubboService.cluster());
        Assert.assertEquals(shenyuDubboService.proxy(), dubboService.proxy());
        Assert.assertEquals(shenyuDubboService.connections(), dubboService.connections());
        Assert.assertEquals(shenyuDubboService.callbacks(), dubboService.callbacks());
        Assert.assertEquals(shenyuDubboService.onconnect(), dubboService.onconnect());
        Assert.assertEquals(shenyuDubboService.ondisconnect(), dubboService.ondisconnect());
        Assert.assertEquals(shenyuDubboService.owner(), dubboService.owner());
        Assert.assertEquals(shenyuDubboService.layer(), dubboService.layer());
        Assert.assertEquals(shenyuDubboService.retries(), dubboService.retries());
        Assert.assertEquals(shenyuDubboService.loadbalance(), dubboService.loadbalance());
        Assert.assertEquals(shenyuDubboService.async(), dubboService.async());
        Assert.assertEquals(shenyuDubboService.actives(), dubboService.actives());
        Assert.assertEquals(shenyuDubboService.sent(), dubboService.sent());
        Assert.assertEquals(shenyuDubboService.mock(), dubboService.mock());
        Assert.assertEquals(shenyuDubboService.validation(), dubboService.validation());
        Assert.assertEquals(shenyuDubboService.timeout(), dubboService.timeout());
        Assert.assertEquals(shenyuDubboService.cache(), dubboService.cache());
        Assert.assertArrayEquals(shenyuDubboService.filter(), dubboService.filter());
        Assert.assertArrayEquals(shenyuDubboService.listener(), dubboService.listener());
        Assert.assertArrayEquals(shenyuDubboService.parameters(), dubboService.parameters());
        Assert.assertEquals(shenyuDubboService.application(), dubboService.application());
        Assert.assertEquals(shenyuDubboService.module(), dubboService.module());
        Assert.assertEquals(shenyuDubboService.provider(), dubboService.provider());
        Assert.assertArrayEquals(shenyuDubboService.protocol(), dubboService.protocol());
        Assert.assertEquals(shenyuDubboService.monitor(), dubboService.monitor());
        Assert.assertArrayEquals(shenyuDubboService.registry(), dubboService.registry());
        Assert.assertEquals(shenyuDubboService.tag(), dubboService.tag());
        Assert.assertArrayEquals(shenyuDubboService.methods(), dubboService.methods());
    }

    @Test
    public void serviceDefaultValue() {
        Service service = ServiceTest.class.getAnnotation(Service.class);
        ShenyuService shenyuService = ShenyuServiceTest.class.getAnnotation(ShenyuService.class);

        Assert.assertEquals(shenyuService.interfaceClass(), service.interfaceClass());
        Assert.assertEquals(shenyuService.interfaceName(), service.interfaceName());
        Assert.assertEquals(shenyuService.version(), service.version());
        Assert.assertEquals(shenyuService.group(), service.group());
        Assert.assertEquals(shenyuService.dubboPath(), service.path());
        Assert.assertEquals(shenyuService.export(), service.export());
        Assert.assertEquals(shenyuService.token(), service.token());
        Assert.assertEquals(shenyuService.deprecated(), service.deprecated());
        Assert.assertEquals(shenyuService.dynamic(), service.dynamic());
        Assert.assertEquals(shenyuService.accesslog(), service.accesslog());
        Assert.assertEquals(shenyuService.executes(), service.executes());
        Assert.assertEquals(shenyuService.register(), service.register());
        Assert.assertEquals(shenyuService.weight(), service.weight());
        Assert.assertEquals(shenyuService.document(), service.document());
        Assert.assertEquals(shenyuService.delay(), service.delay());
        Assert.assertEquals(shenyuService.local(), service.local());
        Assert.assertEquals(shenyuService.stub(), service.stub());
        Assert.assertEquals(shenyuService.cluster(), service.cluster());
        Assert.assertEquals(shenyuService.proxy(), service.proxy());
        Assert.assertEquals(shenyuService.connections(), service.connections());
        Assert.assertEquals(shenyuService.callbacks(), service.callbacks());
        Assert.assertEquals(shenyuService.onconnect(), service.onconnect());
        Assert.assertEquals(shenyuService.ondisconnect(), service.ondisconnect());
        Assert.assertEquals(shenyuService.owner(), service.owner());
        Assert.assertEquals(shenyuService.layer(), service.layer());
        Assert.assertEquals(shenyuService.retries(), service.retries());
        Assert.assertEquals(shenyuService.loadbalance(), service.loadbalance());
        Assert.assertEquals(shenyuService.async(), service.async());
        Assert.assertEquals(shenyuService.actives(), service.actives());
        Assert.assertEquals(shenyuService.sent(), service.sent());
        Assert.assertEquals(shenyuService.mock(), service.mock());
        Assert.assertEquals(shenyuService.validation(), service.validation());
        Assert.assertEquals(shenyuService.timeout(), service.timeout());
        Assert.assertEquals(shenyuService.cache(), service.cache());
        Assert.assertArrayEquals(shenyuService.filter(), service.filter());
        Assert.assertArrayEquals(shenyuService.listener(), service.listener());
        Assert.assertArrayEquals(shenyuService.parameters(), service.parameters());
        Assert.assertEquals(shenyuService.application(), service.application());
        Assert.assertEquals(shenyuService.module(), service.module());
        Assert.assertEquals(shenyuService.provider(), service.provider());
        Assert.assertArrayEquals(shenyuService.protocol(), service.protocol());
        Assert.assertEquals(shenyuService.monitor(), service.monitor());
        Assert.assertArrayEquals(shenyuService.registry(), service.registry());
        Assert.assertEquals(shenyuService.tag(), service.tag());
        Assert.assertArrayEquals(shenyuService.methods(), service.methods());
    }

    @ShenyuDubboService
    private static class ShenyuDubboServiceTest {

    }

    @DubboService
    private static class DubboServiceTest {

    }

    @Service
    private static class ServiceTest {

    }

    @ShenyuService
    private static class ShenyuServiceTest {

    }
}
