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

package org.apache.shenyu.registry.consul;

import com.ecwid.consul.v1.ConsulClient;
import org.junit.jupiter.api.Test;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link TtlScheduler}.
 */
public final class TtlSchedulerTest {

    @Test
    public void heartbeatKeepsBeatingAfterFailure() {
        final ConsulClient client = mock(ConsulClient.class);
        when(client.agentCheckPass(anyString()))
                .thenThrow(new RuntimeException("consul temporarily unavailable"))
                .thenReturn(null);

        final TtlScheduler ttlScheduler = new TtlScheduler(1, client);
        ttlScheduler.add("test-service");

        verify(client, timeout(5000).atLeast(2)).agentCheckPass(eq("service:test-service"));
        ttlScheduler.shutdown();
    }
}
