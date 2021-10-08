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

package org.apache.shenyu.springboot.starter.plugin.apache.dubbo;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.apache.dubbo.ApacheDubboPlugin;
import org.apache.shenyu.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;
import org.apache.shenyu.plugin.apache.dubbo.subscriber.ApacheDubboMetaDataSubscriber;
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
 * Test case for {@link ApacheDubboPluginConfiguration}.
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
public final class ApacheDubboPluginConfigurationTest {

    @Autowired(required = false)
    private ApacheDubboPlugin apacheDubboPlugin;
    
    @Autowired(required = false)
    private ApacheDubboPluginDataHandler apacheDubboPluginDataHandler;
    
    @Autowired(required = false)
    private ApacheDubboMetaDataSubscriber apacheDubboMetaDataSubscriber;

    @Test
    public void testApacheDubboPluginConfiguration() {
        assertNotNull(apacheDubboPlugin);
        assertThat(apacheDubboPlugin.named(), is(PluginEnum.DUBBO.getName()));
        assertThat(apacheDubboPlugin.getOrder(), is(PluginEnum.DUBBO.getCode()));
    }

    @Test
    public void testApacheDubboPluginDataHandlerConfiguration() {
        assertNotNull(apacheDubboPluginDataHandler);
        assertThat(apacheDubboPluginDataHandler.pluginNamed(), is(PluginEnum.DUBBO.getName()));
    }

    @Test
    public void testApacheDubboMetaDataSubscriberConfiguration() {
        assertNotNull(apacheDubboMetaDataSubscriber);
    }
}
