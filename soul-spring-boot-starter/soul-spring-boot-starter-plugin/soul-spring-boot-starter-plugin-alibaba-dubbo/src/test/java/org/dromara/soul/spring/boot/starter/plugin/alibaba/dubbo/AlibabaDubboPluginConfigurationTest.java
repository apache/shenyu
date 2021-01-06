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

package org.dromara.soul.spring.boot.starter.plugin.alibaba.dubbo;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.alibaba.dubbo.AlibabaDubboPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.handler.AlibabaDubboPluginDataHandler;
import org.dromara.soul.plugin.alibaba.dubbo.param.BodyParamPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.response.DubboResponsePlugin;
import org.dromara.soul.plugin.alibaba.dubbo.subscriber.AlibabaDubboMetaDataSubscriber;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Test case for {@link AlibabaDubboPluginConfiguration}.
 *
 * @author: ZhouBin
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                AlibabaDubboPluginConfiguration.class,
                AlibabaDubboPluginConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@EnableAutoConfiguration
public class AlibabaDubboPluginConfigurationTest {

    @Autowired
    private AlibabaDubboPlugin alibabaDubboPlugin;

    @Autowired
    private BodyParamPlugin bodyParamPlugin;

    @Autowired
    private DubboResponsePlugin dubboResponsePlugin;

    @Autowired
    private AlibabaDubboPluginDataHandler alibabaDubboPluginDataHandler;

    @Autowired
    private AlibabaDubboMetaDataSubscriber alibabaDubboMetaDataSubscriber;

    @Test
    public void testAlibabaDubboPlugin() {
        Assert.assertEquals(PluginEnum.DUBBO.getCode(), alibabaDubboPlugin.getOrder());
        Assert.assertEquals(PluginEnum.DUBBO.getName(), alibabaDubboPlugin.named());

        Assert.assertEquals(PluginEnum.DUBBO.getCode() - 1, bodyParamPlugin.getOrder());
        Assert.assertEquals("alibaba-dubbo-body-param", bodyParamPlugin.named());

        Assert.assertEquals(PluginEnum.RESPONSE.getCode(), dubboResponsePlugin.getOrder());
        Assert.assertEquals(PluginEnum.RESPONSE.getName(), dubboResponsePlugin.named());

        Assert.assertEquals(PluginEnum.DUBBO.getName(), alibabaDubboPluginDataHandler.pluginNamed());

        Assert.assertNotNull(alibabaDubboMetaDataSubscriber);
    }
}
