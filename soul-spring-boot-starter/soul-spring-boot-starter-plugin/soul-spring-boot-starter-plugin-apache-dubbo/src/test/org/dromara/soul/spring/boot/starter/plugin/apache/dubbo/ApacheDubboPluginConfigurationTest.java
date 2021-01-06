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

package org.dromara.soul.spring.boot.starter.plugin.apache.dubbo;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.apache.dubbo.ApacheDubboPlugin;
import org.dromara.soul.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;
import org.dromara.soul.plugin.apache.dubbo.param.BodyParamPlugin;
import org.dromara.soul.plugin.apache.dubbo.response.DubboResponsePlugin;
import org.dromara.soul.plugin.apache.dubbo.subscriber.ApacheDubboMetaDataSubscriber;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Test case for {@link ApacheDubboPluginConfiguration}.
 *
 * @author lahmxu
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                ApacheDubboPluginConfiguration.class,
                ApacheDubboPluginConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@EnableAutoConfiguration
public class ApacheDubboPluginConfigurationTest {

    @Autowired
    private ApacheDubboPlugin apacheDubboPlugin;

    @Autowired
    private BodyParamPlugin bodyParamPlugin;

    @Autowired
    private DubboResponsePlugin dubboResponsePlugin;

    @Autowired
    private ApacheDubboPluginDataHandler apacheDubboPluginDataHandler;

    @Autowired
    private ApacheDubboMetaDataSubscriber apacheDubboMetaDataSubscriber;

    @Test
    public void testApacheDubboPluginConfiguration() {
        assertNotNull(apacheDubboPlugin);
        assertEquals(PluginEnum.DUBBO.getName(), apacheDubboPlugin.named());
        assertEquals(PluginEnum.DUBBO.getCode(), apacheDubboPlugin.getOrder());
    }

    @Test
    public void testBodyParamPluginConfiguration() {
        assertNotNull(bodyParamPlugin);
        assertEquals("apache-dubbo-body-param", bodyParamPlugin.named());
        assertEquals(PluginEnum.DUBBO.getCode() - 1, bodyParamPlugin.getOrder());
    }

    @Test
    public void testDubboResponsePluginConfiguration() {
        assertNotNull(dubboResponsePlugin);
        assertEquals(PluginEnum.RESPONSE.getCode(), dubboResponsePlugin.getOrder());
        assertEquals(PluginEnum.RESPONSE.getName(), dubboResponsePlugin.named());
    }

    @Test
    public void testApacheDubboPluginDataHandlerConfiguration() {
        assertNotNull(apacheDubboPluginDataHandler);
        assertEquals(PluginEnum.DUBBO.getName(), apacheDubboPluginDataHandler.pluginNamed());
    }

    @Test
    public void testApacheDubboMetaDataSubscriberConfiguration() {
        assertNotNull(apacheDubboMetaDataSubscriber);
    }
}
