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

package org.apache.shenyu.registry.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import com.ctrip.framework.apollo.enums.PropertyChangeType;
import com.ctrip.framework.apollo.model.ConfigChange;
import com.ctrip.framework.apollo.model.ConfigChangeEvent;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class ApolloInstanceRegisterRepositoryTest {

    private ApolloInstanceRegisterRepository repository;

    private ConfigChangeListener listener;

    private String instanceKey;

    private String originalValue;

    @BeforeEach
    public void setUp() {
        repository = new ApolloInstanceRegisterRepository();
        Config config = mock(Config.class);
        ApolloClient client = mock(ApolloClient.class);
        ReflectionTestUtils.setField(repository, "configService", config);
        ReflectionTestUtils.setField(repository, "apolloClient", client);
        instanceKey = InstancePathConstants.buildRealNode(InstancePathConstants.buildInstanceParentPath("service"), "instance");
        originalValue = instanceJson("127.0.0.1", 8080);
        when(config.getPropertyNames()).thenReturn(Collections.singleton(instanceKey));
        when(client.getItemValue(instanceKey)).thenReturn(originalValue);
        assertEquals("127.0.0.1", repository.selectInstances("service").get(0).getHost());
        ArgumentCaptor<ConfigChangeListener> captor = ArgumentCaptor.forClass(ConfigChangeListener.class);
        verify(config).addChangeListener(captor.capture());
        listener = captor.getValue();
    }

    @Test
    public void testModifiedInstanceRefreshesCache() {
        publishChange(instanceKey, originalValue, instanceJson("127.0.0.2", 9090), PropertyChangeType.MODIFIED);
        List<InstanceEntity> instances = repository.selectInstances("service");
        assertEquals(1, instances.size());
        assertEquals("127.0.0.2", instances.get(0).getHost());
        assertEquals(9090, instances.get(0).getPort());
        assertEquals(URI.create("http://127.0.0.2:9090"), instances.get(0).getUri());
    }

    @Test
    public void testAddedAndDeletedInstancesRefreshCache() {
        String addedKey = instanceKey + "-second";
        String addedValue = instanceJson("127.0.0.2", 9090);
        publishChange(addedKey, null, addedValue, PropertyChangeType.ADDED);
        assertEquals(2, repository.selectInstances("service").size());
        publishChange(instanceKey, originalValue, null, PropertyChangeType.DELETED);
        assertEquals("127.0.0.2", repository.selectInstances("service").get(0).getHost());
        publishChange(addedKey, addedValue, null, PropertyChangeType.DELETED);
        assertTrue(repository.selectInstances("service").isEmpty());
    }

    @Test
    public void testUnrelatedServiceChangeDoesNotAffectCache() {
        String unrelatedKey = InstancePathConstants.buildRealNode(InstancePathConstants.buildInstanceParentPath("another-service"), "instance");
        publishChange(unrelatedKey, originalValue, instanceJson("127.0.0.2", 9090), PropertyChangeType.MODIFIED);
        List<InstanceEntity> instances = repository.selectInstances("service");
        assertEquals(1, instances.size());
        assertEquals("127.0.0.1", instances.get(0).getHost());
    }

    private void publishChange(final String key, final String oldValue, final String newValue, final PropertyChangeType type) {
        ConfigChange change = new ConfigChange("application", key, oldValue, newValue, type);
        listener.onChange(new ConfigChangeEvent("application", Collections.singletonMap(key, change)));
    }

    private String instanceJson(final String host, final int port) {
        return GsonUtils.getInstance().toJson(InstanceEntity.builder().appName("service").host(host).port(port).build());
    }
}
