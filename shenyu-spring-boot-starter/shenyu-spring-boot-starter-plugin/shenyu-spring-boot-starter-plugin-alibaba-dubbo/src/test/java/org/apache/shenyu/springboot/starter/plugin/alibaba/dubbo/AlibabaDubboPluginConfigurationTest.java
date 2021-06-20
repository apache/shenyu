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

package org.apache.shenyu.springboot.starter.plugin.alibaba.dubbo;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.alibaba.dubbo.AlibabaDubboPlugin;
import org.apache.shenyu.plugin.alibaba.dubbo.handler.AlibabaDubboPluginDataHandler;
import org.apache.shenyu.plugin.alibaba.dubbo.subscriber.AlibabaDubboMetaDataSubscriber;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

/**
 * Test case for {@link AlibabaDubboPluginConfiguration}.
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
public final class AlibabaDubboPluginConfigurationTest {
    
    @Autowired(required = false)
    private AlibabaDubboPlugin alibabaDubboPlugin;
    
    @Autowired(required = false)
    private AlibabaDubboPluginDataHandler alibabaDubboPluginDataHandler;
    
    @Autowired(required = false)
    private AlibabaDubboMetaDataSubscriber alibabaDubboMetaDataSubscriber;

    @Test
    public void testAlibabaDubboPlugin() {
        assertThat(alibabaDubboPlugin.getOrder(), is(PluginEnum.DUBBO.getCode()));
        assertThat(alibabaDubboPlugin.named(), is(PluginEnum.DUBBO.getName()));
        assertThat(alibabaDubboPluginDataHandler.pluginNamed(), is(PluginEnum.DUBBO.getName()));
        assertNotNull(alibabaDubboMetaDataSubscriber);
    }
}
