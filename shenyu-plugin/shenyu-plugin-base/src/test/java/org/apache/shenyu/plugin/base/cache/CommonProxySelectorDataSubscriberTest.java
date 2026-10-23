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

package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.plugin.base.handler.ProxySelectorDataHandler;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class CommonProxySelectorDataSubscriberTest {

    @Test
    public void testRefresh() {
        ProxySelectorDataHandler firstHandler = mock(ProxySelectorDataHandler.class);
        ProxySelectorDataHandler secondHandler = mock(ProxySelectorDataHandler.class);
        when(firstHandler.pluginName()).thenReturn("first");
        when(secondHandler.pluginName()).thenReturn("second");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Arrays.asList(firstHandler, secondHandler));

        subscriber.refresh();

        verify(firstHandler).refresh();
        verify(secondHandler).refresh();
    }

    @Test
    public void testRefreshWithoutHandlers() {
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.emptyList());

        assertDoesNotThrow(subscriber::refresh);
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testDeletionByIdOrFullData(final boolean fullData) {
        ProxySelectorDataHandler handler = mock(ProxySelectorDataHandler.class);
        when(handler.pluginName()).thenReturn("tcp");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(handler));
        subscriber.onSubscribe(data("tcp", "selector-id", "listener-name"));

        subscriber.unSubscribe(data("tcp", "selector-id", fullData ? "listener-name" : null));
        subscriber.unSubscribe(data("tcp", "selector-id", null));

        verify(handler).removeProxySelector("listener-name");
        verify(handler).removeProxySelector(any());
    }

    @Test
    public void testPluginIdentitiesAreIndependent() {
        ProxySelectorDataHandler first = mock(ProxySelectorDataHandler.class);
        ProxySelectorDataHandler second = mock(ProxySelectorDataHandler.class);
        when(first.pluginName()).thenReturn("first");
        when(second.pluginName()).thenReturn("second");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Arrays.asList(first, second));
        subscriber.onSubscribe(data("first", "same-id", "first-name"));
        subscriber.onSubscribe(data("second", "same-id", "second-name"));

        subscriber.unSubscribe(data("first", "same-id", null));
        verify(first).removeProxySelector("first-name");
        verify(second, never()).removeProxySelector(any());
        subscriber.unSubscribe(data("second", "same-id", null));
        verify(second).removeProxySelector("second-name");
    }

    @Test
    public void testFailedDeletionCanRetryById() {
        ProxySelectorDataHandler handler = mock(ProxySelectorDataHandler.class);
        when(handler.pluginName()).thenReturn("tcp");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(handler));
        subscriber.onSubscribe(data("tcp", "selector-id", "listener-name"));
        doThrow(new IllegalStateException("temporary shutdown failure")).doNothing().when(handler).removeProxySelector("listener-name");

        assertThrows(IllegalStateException.class, () -> subscriber.unSubscribe(data("tcp", "selector-id", null)));
        assertDoesNotThrow(() -> subscriber.unSubscribe(data("tcp", "selector-id", null)));
        verify(handler, times(2)).removeProxySelector("listener-name");
    }

    @Test
    public void testFailedUpdateRetainsAppliedName() {
        ProxySelectorDataHandler handler = mock(ProxySelectorDataHandler.class);
        when(handler.pluginName()).thenReturn("tcp");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(handler));
        subscriber.onSubscribe(data("tcp", "selector-id", "listener-name"));
        ProxySelectorData update = data("tcp", "selector-id", "failed-name");
        doThrow(new IllegalStateException("listener creation failed")).when(handler).handlerProxySelector(update);

        assertThrows(IllegalStateException.class, () -> subscriber.onSubscribe(update));
        subscriber.unSubscribe(data("tcp", "selector-id", null));
        verify(handler).removeProxySelector("listener-name");
    }

    @Test
    public void testRefreshDiscardsRememberedNames() {
        ProxySelectorDataHandler handler = mock(ProxySelectorDataHandler.class);
        when(handler.pluginName()).thenReturn("tcp");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(handler));
        subscriber.onSubscribe(data("tcp", "selector-id", "listener-name"));

        subscriber.refresh();
        subscriber.unSubscribe(data("tcp", "selector-id", null));

        verify(handler).refresh();
        verify(handler, never()).removeProxySelector(any());
    }

    @Test
    public void testLegacyNameOnlyDeletion() {
        ProxySelectorDataHandler handler = mock(ProxySelectorDataHandler.class);
        when(handler.pluginName()).thenReturn("tcp");
        CommonProxySelectorDataSubscriber subscriber = new CommonProxySelectorDataSubscriber(Collections.singletonList(handler));
        subscriber.onSubscribe(data("tcp", "selector-id", "legacy-name"));

        subscriber.unSubscribe(data("tcp", null, "legacy-name"));
        subscriber.unSubscribe(data("tcp", "selector-id", null));

        verify(handler).removeProxySelector("legacy-name");
        verify(handler).removeProxySelector(any());
    }

    private static ProxySelectorData data(final String plugin, final String id, final String name) {
        ProxySelectorData data = new ProxySelectorData();
        data.setPluginName(plugin);
        data.setId(id);
        data.setName(name);
        return data;
    }
}
