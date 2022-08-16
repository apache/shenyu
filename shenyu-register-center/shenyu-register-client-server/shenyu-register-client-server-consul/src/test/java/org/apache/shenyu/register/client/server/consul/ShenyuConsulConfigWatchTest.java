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

package org.apache.shenyu.register.client.server.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The TestCase for {@link ShenyuConsulConfigWatch}.
 */
public class ShenyuConsulConfigWatchTest {

    @Test
    public void testShenyuConsulConfigWatch() throws NoSuchFieldException, IllegalAccessException {
        final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();
        final ApplicationEventPublisher applicationEventPublisher = mock(ApplicationEventPublisher.class);
        final ShenyuConsulConfigWatch shenyuConsulConfigWatch = new ShenyuConsulConfigWatch(shenyuRegisterCenterConfig, applicationEventPublisher);
        shenyuConsulConfigWatch.start();
        shenyuConsulConfigWatch.start();

        shenyuConsulConfigWatch.stop();

        shenyuConsulConfigWatch.start();
        final Field watchFuture = ShenyuConsulConfigWatch.class.getDeclaredField("watchFuture");
        watchFuture.setAccessible(true);
        watchFuture.set(shenyuConsulConfigWatch, null);
        shenyuConsulConfigWatch.stop();
        shenyuConsulConfigWatch.stop();
        shenyuConsulConfigWatch.isRunning();
    }

    @Test
    public void testWatchConfigKeyValues() throws NoSuchMethodException, IllegalAccessException, NoSuchFieldException {
        final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();
        final ApplicationEventPublisher applicationEventPublisher = mock(ApplicationEventPublisher.class);
        final ShenyuConsulConfigWatch shenyuConsulConfigWatch = new ShenyuConsulConfigWatch(shenyuRegisterCenterConfig, applicationEventPublisher);
        shenyuConsulConfigWatch.start();

        final Method watchConfigKeyValues = ShenyuConsulConfigWatch.class.getDeclaredMethod("watchConfigKeyValues");
        watchConfigKeyValues.setAccessible(true);

        final Field consul = ShenyuConsulConfigWatch.class.getDeclaredField("consul");
        consul.setAccessible(true);
        final ConsulClient consulClient = mock(ConsulClient.class);
        consul.set(shenyuConsulConfigWatch, consulClient);
        final Response<List<GetValue>> response = mock(Response.class);

        when(consulClient.getKVValues(any(), any(), any())).thenReturn(response);

        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));

        List<GetValue> getValues = new ArrayList<>(1);
        getValues.add(mock(GetValue.class));
        when(response.getValue()).thenReturn(getValues);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));

        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));

        when(response.getConsulIndex()).thenReturn(null);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));

        when(response.getConsulIndex()).thenReturn(0L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));

        final Field consulIndexes = ShenyuConsulConfigWatch.class.getDeclaredField("consulIndexes");
        consulIndexes.setAccessible(true);
        final Map<String, Long> consulIndexesSource = (Map<String, Long>) consulIndexes.get(shenyuConsulConfigWatch);
        consulIndexesSource.put("/null", null);
        when(response.getConsulIndex()).thenReturn(2L);
        Assertions.assertDoesNotThrow(() -> watchConfigKeyValues.invoke(shenyuConsulConfigWatch));
    }
}
