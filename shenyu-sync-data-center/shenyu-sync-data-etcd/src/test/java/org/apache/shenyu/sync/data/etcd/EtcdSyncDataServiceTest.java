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

import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * test case for {@link EtcdSyncDataService}.
 */
@ExtendWith(MockitoExtension.class)
public class EtcdSyncDataServiceTest {

    private EtcdSyncDataService etcdSyncDataService;

    @Mock
    private EtcdClient etcdClient;


    @Test
    public void ruleTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.RULE, "/shenyu/rule/divide/selectorId-ruleId");
    }

    @Test
    public void selectorTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.SELECTOR, "/shenyu/selector/divide/selectorId");
    }

    @Test
    public void metaDataTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.META_DATA, "/shenyu/metaData/divide/metaDataId");
    }

    @Test
    public void discoverUpstreamTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.DISCOVER_UPSTREAM, "/shenyu/discoveryUpstream/divide/id");
    }

    @Test
    public void proxySelectorTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.PROXY_SELECTOR, "/shenyu/proxySelectorData/divide/id");
    }

    @Test
    public void appAuthTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.APP_AUTH, "/shenyu/appAuth/divide/appAuthId");
    }

    /**
    * commonTest.
    * @param configGroupEnum configGroupEnum
    * @param key key
    * @throws NoSuchMethodException NoSuchMethodException
    * @throws InvocationTargetException InvocationTargetException
    * @throws IllegalAccessException IllegalAccessException
    * @throws NoSuchFieldException NoSuchFieldException
    */
    public void commonTest(final ConfigGroupEnum configGroupEnum, final String key) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        final EtcdClient mockEtcdClient = mock(EtcdClient.class);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient,
                mock(PluginDataSubscriber.class),
                Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        final Field etcdClient = EtcdSyncDataService.class.getDeclaredField("etcdClient");
        etcdClient.setAccessible(true);
        etcdClient.set(etcdSyncDataService, mockEtcdClient);
        final Method subscribeChildChanges = EtcdSyncDataService.class.getDeclaredMethod("subscribeChildChanges", ConfigGroupEnum.class, String.class);
        subscribeChildChanges.setAccessible(true);

        final List<BiConsumer<String, String>> biConsumers = new ArrayList<>(4);
        doAnswer(invocation -> {
            biConsumers.add(invocation.getArgument(1));
            return true;
        }).when(mockEtcdClient).watchChildChange(anyString(), any(), any());

        final List<BiConsumer<String, String>> dataBiConsumers = new ArrayList<>(4);
        final List<Consumer<String>> dataConsumers = new ArrayList<>(4);
        doAnswer(invocation -> {
            dataBiConsumers.add(invocation.getArgument(1));
            dataConsumers.add(invocation.getArgument(2));
            return true;
        }).when(mockEtcdClient).watchDataChange(anyString(), any(), any());

        subscribeChildChanges.invoke(etcdSyncDataService, configGroupEnum, "groupParentPath");
        // hit default
        assertThrows(InvocationTargetException.class, () -> subscribeChildChanges.invoke(etcdSyncDataService, ConfigGroupEnum.PLUGIN, "groupParentPath"));

        biConsumers.forEach(biConsumer -> {
            biConsumer.accept("updatePath", "{}");
        });
        dataConsumers.forEach(consumer -> {
            consumer.accept(key);
        });
        dataBiConsumers.forEach(biConsumer -> {
            biConsumer.accept("updatePath", "{}");
        });
        mockEtcdClient.close();
    }
}
