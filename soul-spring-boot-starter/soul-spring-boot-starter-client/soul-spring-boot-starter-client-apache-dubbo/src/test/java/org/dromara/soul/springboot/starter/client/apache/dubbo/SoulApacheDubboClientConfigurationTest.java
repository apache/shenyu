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

package org.dromara.soul.springboot.starter.client.apache.dubbo;

import org.dromara.soul.client.dubbo.common.config.DubboConfig;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Test case for {@link SoulApacheDubboClientConfiguration}.
 *
 * @author HoldDie
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                SoulApacheDubboClientConfiguration.class,
                SoulApacheDubboClientConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.dubbo.adminUrl=http://localhost:9095",
                "soul.dubbo.contextPath=/dubbo",
                "soul.dubbo.appName=dubbo"
        })
@EnableAutoConfiguration
public final class SoulApacheDubboClientConfigurationTest {

    @Autowired
    private DubboConfig dubboConfig;

    @Test
    public void testDubboConfig() {
        assertThat(dubboConfig.getAppName(), is("dubbo"));
        assertThat(dubboConfig.getContextPath(), is("/dubbo"));
        assertThat(dubboConfig.getAdminUrl(), is("http://localhost:9095"));
    }
}
