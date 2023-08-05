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

package org.apache.shenyu.sync.data.apollo;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.internals.DefaultConfig;
import com.ctrip.framework.apollo.internals.LocalFileConfigRepository;
import org.apache.shenyu.common.constant.ListDataNodePathConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

@ContextConfiguration(classes = {ApolloDataServiceTest.class})
@RunWith(SpringJUnit4ClassRunner.class)
public class ApolloDataServiceTest {


    /**
     * Method under test: {@link ApolloDataService#ApolloDataService(Config, PluginDataSubscriber, List, List, List, List)}.
     */
    @Test
    public void testConstructor() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberAuthData(apolloDataService.readData(ListDataNodePathConstants.AUTH_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#ApolloDataService(Config, PluginDataSubscriber, List, List, List, List)}.
     */
    @Test
    public void testConstructor2() {
        DefaultConfig configService = new DefaultConfig("Namespace",
                new LocalFileConfigRepository("shenyu.auth.json", new LocalFileConfigRepository("shenyu.auth.json")));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberAuthData(apolloDataService.readData(ListDataNodePathConstants.SELECTOR_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subAllData()}.
     */
    @Test
    public void testSubAllData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        apolloDataService.subAllData();
        assertTrue(apolloDataService.subscriberAuthData(apolloDataService.readData(ListDataNodePathConstants.AUTH_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberProxySelectorData(apolloDataService.readData(ListDataNodePathConstants.PROXY_SELECTOR_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberPluginData(apolloDataService.readData(ListDataNodePathConstants.PLUGIN_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberMetaData(apolloDataService.readData(ListDataNodePathConstants.META_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberDiscoverySyncData(apolloDataService.readData(ListDataNodePathConstants.DISCOVERY_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberPluginData(String)} ()}.
     */
    @Test
    public void testSubscriberPluginData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberPluginData(apolloDataService.readData(ListDataNodePathConstants.PLUGIN_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberSelectorData(String)} ()}.
     */
    @Test
    public void testSubscriberSelectorData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberSelectorData(apolloDataService.readData(ListDataNodePathConstants.META_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberRuleData(String)} ()}.
     */
    @Test
    public void testSubscriberRuleData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberRuleData(apolloDataService.readData(ListDataNodePathConstants.RULE_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberMetaData(String)} ()}.
     */
    @Test
    public void testSubscriberMetaData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberMetaData(apolloDataService.readData(ListDataNodePathConstants.META_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberProxySelectorData(String)} ()}.
     */
    @Test
    public void testSubscriberProxySelectorData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberProxySelectorData(apolloDataService.readData(ListDataNodePathConstants.SELECTOR_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#subscriberDiscoverySyncData(String)} ()}.
     */
    @Test
    public void testSubscriberDiscoverySyncData() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        assertTrue((new ApolloDataService(configService, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers,
                proxySelectorDataSubscribers, new ArrayList<>())).subscriberDiscoverySyncData(apolloDataService.readData(ListDataNodePathConstants.DISCOVERY_DATA_ID)).isEmpty());
    }

    /**
     * Method under test: {@link ApolloDataService#close()}.
     */
    @Test
    public void testClose() {
        DefaultConfig configService = new DefaultConfig("Namespace", new LocalFileConfigRepository("Namespace"));

        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        ArrayList<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        ArrayList<AuthDataSubscriber> authDataSubscribers = new ArrayList<>();
        ArrayList<ProxySelectorDataSubscriber> proxySelectorDataSubscribers = new ArrayList<>();
        ApolloDataService apolloDataService = new ApolloDataService(configService, pluginDataSubscriber,
                metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, new ArrayList<>());
        apolloDataService.close();
        assertTrue(apolloDataService.subscriberAuthData(apolloDataService.readData(ListDataNodePathConstants.AUTH_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberProxySelectorData(apolloDataService.readData(ListDataNodePathConstants.PROXY_SELECTOR_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberPluginData(apolloDataService.readData(ListDataNodePathConstants.PLUGIN_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberMetaData(apolloDataService.readData(ListDataNodePathConstants.META_DATA_ID)).isEmpty());
        assertTrue(apolloDataService.subscriberDiscoverySyncData(apolloDataService.readData(ListDataNodePathConstants.DISCOVERY_DATA_ID)).isEmpty());
    }
}

