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

package org.apache.shenyu.sync.data.etcd;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.infra.etcd.client.EtcdClient;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link EtcdSyncDataService}.
 */
@ExtendWith(MockitoExtension.class)
public class EtcdSyncDataServiceTest {

    @Test
    public void testZookeeperInstanceRegisterRepository() throws Exception {

        EtcdClient etcdClient = mock(EtcdClient.class);
        PluginDataSubscriber pluginDataSubscriber = mock(PluginDataSubscriber.class);
        doAnswer(invocationOnMock -> {
            BiConsumer<String, String> updateHandler = invocationOnMock.getArgument(1);
            updateHandler.accept("updateData", "{}");
            Consumer<String> deleteHandler = invocationOnMock.getArgument(2);
            String pluginName = invocationOnMock.getArgument(0);
            deleteHandler.accept(pluginName + "removeKey");
            return null;
        }).when(etcdClient).watchChildChange(any(), any(), any());
        when(etcdClient.getChildrenKeys(any(), any())).thenReturn(Collections.singletonList("test"));
        final AuthDataSubscriber authDataSubscriber = mock(AuthDataSubscriber.class);
        final MetaDataSubscriber metaDataSubscriber = mock(MetaDataSubscriber.class);
        final ProxySelectorDataSubscriber proxySelectorDataSubscriber = mock(ProxySelectorDataSubscriber.class);
        final DiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        final ShenyuConfig shenyuConfig = mock(ShenyuConfig.class);
        when(shenyuConfig.getNamespace()).thenReturn(Constants.SYS_DEFAULT_NAMESPACE_ID);
        final EtcdSyncDataService zookeeperSyncDataService = new EtcdSyncDataService(shenyuConfig, etcdClient,
                pluginDataSubscriber, Collections.singletonList(metaDataSubscriber), Collections.singletonList(authDataSubscriber),
                Collections.singletonList(proxySelectorDataSubscriber), Collections.singletonList(discoveryUpstreamDataSubscriber));

        zookeeperSyncDataService.close();
    }
}
