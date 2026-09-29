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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import java.util.Arrays;
import java.util.Collections;

import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;

public final class DiscoveryUpstreamDataHandlerTest {

    @Test
    public void testDoRefresh() {
        DiscoveryUpstreamDataSubscriber firstSubscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        DiscoveryUpstreamDataSubscriber secondSubscriber = mock(DiscoveryUpstreamDataSubscriber.class);
        DiscoveryUpstreamDataHandler handler = new DiscoveryUpstreamDataHandler(Arrays.asList(firstSubscriber, secondSubscriber));
        DiscoverySyncData data = new DiscoverySyncData();

        handler.doRefresh(Collections.singletonList(data));

        InOrder inOrder = inOrder(firstSubscriber, secondSubscriber);
        inOrder.verify(firstSubscriber).refresh();
        inOrder.verify(secondSubscriber).refresh();
        inOrder.verify(firstSubscriber).onSubscribe(data);
        inOrder.verify(secondSubscriber).onSubscribe(data);
    }
}
