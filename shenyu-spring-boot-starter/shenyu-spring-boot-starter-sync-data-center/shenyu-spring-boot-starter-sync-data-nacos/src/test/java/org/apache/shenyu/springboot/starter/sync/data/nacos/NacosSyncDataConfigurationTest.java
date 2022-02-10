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

package org.apache.shenyu.springboot.starter.sync.data.nacos;

import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.nacos.config.NacosConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.mockito.Answers.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * The test case for {@link NacosSyncDataConfiguration}.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(
        classes = NacosSyncDataConfiguration.class,
        properties = {
                "shenyu.sync.nacos.url=localhost:8848",
                "shenyu.sync.nacos.namespace=1c10d748-af86-43b9-8265-75f487d20c6c"
        })
@EnableAutoConfiguration
@MockBean(name = "nacosConfigService", value = NacosMockConfigService.class, answer = CALLS_REAL_METHODS)
public final class NacosSyncDataConfigurationTest {

    @Autowired
    private SyncDataService syncDataService;

    @Autowired
    private NacosConfig nacosConfig;

    @Test
    public void nacosSyncDataServiceTest() {
        assertNotNull(syncDataService);
    }

    @Test
    public void nacosConfigTest() {
        assertNotNull(nacosConfig);
    }
}
