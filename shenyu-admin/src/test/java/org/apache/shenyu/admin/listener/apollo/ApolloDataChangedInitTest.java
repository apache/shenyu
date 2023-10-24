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

package org.apache.shenyu.admin.listener.apollo;

import static org.apache.shenyu.common.constant.NacosPathConstants.AUTH_DATA_ID;
import static org.apache.shenyu.common.constant.NacosPathConstants.PLUGIN_DATA_ID;
import static org.apache.shenyu.common.constant.NacosPathConstants.META_DATA_ID;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import org.apache.shenyu.admin.config.properties.ApolloProperties;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.runner.RunWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration(classes = {ApolloDataChangedInit.class})
@RunWith(SpringJUnit4ClassRunner.class)
public class ApolloDataChangedInitTest {

    @MockBean
    private ApolloClient apolloClient;

    @MockBean
    private ApolloDataChangedInit apolloDataChangedInit;

    /**
     * Method under test: {@link ApolloDataChangedInit#ApolloDataChangedInit(ApolloClient)}.
     */
    @Test
    public void testConstructor() {
        ApolloProperties apolloConfig = new ApolloProperties();
        apolloConfig.setAppId("42");
        apolloConfig.setClusterName("Cluster Name");
        apolloConfig.setEnv("Env");
        apolloConfig.setMeta("Meta");
        apolloConfig.setNamespace("Namespace");
        apolloConfig.setPortalUrl("http://localhost:8080");
        apolloConfig.setToken("ABC123");
        ApolloClient apolloClient = new ApolloClient(apolloConfig);
        ApolloDataChangedInit actualApolloDataChangedInit = new ApolloDataChangedInit(apolloClient);
        assertNotNull(actualApolloDataChangedInit);
    }

    /**
     * Method under test: {@link ApolloDataChangedInit#notExist()}.
     */
    @Test
    public void testNotExist() {

        when(apolloClient.getItemValue(PLUGIN_DATA_ID)).thenReturn(PLUGIN_DATA_ID);
        boolean pluginExist = apolloDataChangedInit.notExist();
        Assertions.assertFalse(pluginExist, "plugin exist.");
        when(apolloClient.getItemValue(PLUGIN_DATA_ID)).thenReturn(null);

        when(apolloClient.getItemValue(AUTH_DATA_ID)).thenReturn(AUTH_DATA_ID);
        boolean authExist = apolloDataChangedInit.notExist();
        Assertions.assertFalse(authExist, "auth exist.");
        when(apolloClient.getItemValue(AUTH_DATA_ID)).thenReturn(null);

        when(apolloClient.getItemValue(META_DATA_ID)).thenReturn(META_DATA_ID);
        boolean metaDataExist = apolloDataChangedInit.notExist();
        Assertions.assertFalse(metaDataExist, "metadata exist.");
        when(apolloClient.getItemValue(META_DATA_ID)).thenReturn(null);

    }
}

