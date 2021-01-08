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

package org.dromara.soul.springboot.starter.client.springmvc;

import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Test case for {@link SoulSpringMvcClientConfiguration}.
 *
 * @author Wu YuHao
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                SoulSpringMvcClientConfiguration.class,
                SoulSpringMvcClientConfigurationTest.class
        },
        properties = {
                "soul.http.adminUrl=http://localhost:9095",
                "soul.http.contextPath=/http",
                "soul.http.appName=http",
                "soul.http.full=false",
                "soul.http.port=9190",
                "soul.http.host=10.200.1.112"

        })
@EnableAutoConfiguration
public final class SoulSpringMvcClientConfigurationTest {

    @Autowired
    private SoulSpringMvcConfig soulSpringMvcConfig;

    @Test
    public void testSpringMvcConfig() {

        Assert.assertEquals("http", soulSpringMvcConfig.getAppName());
        Assert.assertEquals("/http", soulSpringMvcConfig.getContextPath());
        Assert.assertEquals("http://localhost:9095", soulSpringMvcConfig.getAdminUrl());
        Assert.assertEquals(Integer.valueOf(9190), soulSpringMvcConfig.getPort());
        Assert.assertEquals("10.200.1.112", soulSpringMvcConfig.getHost());
        Assert.assertFalse(soulSpringMvcConfig.isFull());

    }
}
