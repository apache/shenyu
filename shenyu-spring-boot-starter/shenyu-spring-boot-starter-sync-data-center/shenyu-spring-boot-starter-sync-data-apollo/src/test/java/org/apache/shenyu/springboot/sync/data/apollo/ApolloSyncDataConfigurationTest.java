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

package org.apache.shenyu.springboot.sync.data.apollo;

import com.ctrip.framework.apollo.ConfigService;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.apollo.ApolloDataService;
import org.apache.shenyu.sync.data.apollo.config.ApolloConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Answers.CALLS_REAL_METHODS;

@ExtendWith(SpringExtension.class)
@MockBean(ConfigService.class)
@EnableAutoConfiguration
@SpringBootTest(
        classes = {
                ApolloSyncDataConfiguration.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
        properties = {
                "shenyu.sync.apollo.meta=http://localhost:8080",
                "shenyu.sync.apollo.appId=shenyu",
                "shenyu.sync.apollo.env=dev",
                "shenyu.sync.apollo.clusterName=test",
                "shenyu.sync.apollo.namespace=application"

        })
@MockBean(name = "shenyuConfig", value = ShenyuConfig.class, answer = CALLS_REAL_METHODS)
class ApolloSyncDataConfigurationTest {

    @Autowired
    private ApolloConfig apolloConfig;

    @Autowired
    private SyncDataService syncDataService;

    /**
     * case to test {@link ApolloSyncDataConfiguration} to register bean {@link ApolloSyncDataConfiguration}.
     */
    @Test
    public void testApolloSyncDataConfigurationRegisterBeanSyncDataService() {
        assertNotNull(syncDataService);
        assertTrue(syncDataService instanceof ApolloDataService);
    }

    /**
     * case to test {@link ApolloSyncDataConfiguration} to register bean {@link ApolloSyncDataConfiguration}.
     */
    @Test
    public void testApolloSyncDataConfigurationRegisterBeanApolloConfig() {
        assertThat(apolloConfig.getMeta(), is("http://localhost:8080"));
        assertThat(apolloConfig.getAppId(), is("shenyu"));
        assertThat(apolloConfig.getEnv(), is("dev"));
        assertThat(apolloConfig.getClusterName(), is("test"));
        assertThat(apolloConfig.getNamespace(), is("application"));

    }
}

