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

package org.dromara.soul.springboot.starter.client.alibaba.dubbo;

import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Test case for {@link SoulAlibabaDubboClientConfiguration}.
 *
 * @author jmwasky
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                SoulAlibabaDubboClientConfiguration.class,
                SoulAlibabaDubboClientConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "soul.client.registerType = http",
                "soul.client.props.contextPath=/dubbo",
                "soul.client.props.appName=dubbo"
        })
@EnableAutoConfiguration
public final class SoulAlibabaDubboClientConfigurationTest {

    @Autowired
    private SoulRegisterCenterConfig centerConfig;

    @Test
    public void testSoulRegisterCenterConfig() {
        assertThat(centerConfig.getRegisterType(), is("http"));
    }
}
