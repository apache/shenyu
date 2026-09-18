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

import org.apache.shenyu.plugin.base.handler.ProxySelectorDataHandler;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
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
}
