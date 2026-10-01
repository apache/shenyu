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

import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public final class CommonDiscoveryUpstreamDataSubscriberTest {

    @Test
    public void testUnSubscribe() {
        DiscoveryUpstreamDataHandler handler = mock(DiscoveryUpstreamDataHandler.class);
        when(handler.pluginName()).thenReturn("divide");
        CommonDiscoveryUpstreamDataSubscriber subscriber = new CommonDiscoveryUpstreamDataSubscriber(Collections.singletonList(handler));
        DiscoveryUpstreamKey key = new DiscoveryUpstreamKey("divide", "selector-id", null);

        subscriber.unSubscribe(key);

        verify(handler).removeDiscoveryUpstreamData(key);
    }

}
