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

package org.apache.shenyu.protocol.tcp.connection;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.sql.Timestamp;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link DefaultConnectionConfigProvider}.
 */
public final class DefaultConnectionConfigProviderTest {

    private static final String SELECTOR = "tcp-selector";

    @AfterEach
    public void cleanUpstreams() {
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.emptyList());
    }

    @Test
    public void getProxiedServiceShouldBuildUriFromSelectedUpstream() {
        DiscoveryUpstreamData upstream = DiscoveryUpstreamData.builder()
                .protocol("tcp")
                .url("127.0.0.1:20000")
                .status(0)
                .weight(1)
                .props("{\"warmupTime\":0}")
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .build();
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.singletonList(upstream));
        DefaultConnectionConfigProvider provider =
                new DefaultConnectionConfigProvider("random", SELECTOR);

        URI proxiedService = provider.getProxiedService("127.0.0.1");

        assertEquals(URI.create("tcp://127.0.0.1:20000"), proxiedService);
    }

    @Test
    public void getProxiedServiceShouldThrowWhenNoUpstreamExists() {
        UpstreamProvider.getSingleton().createUpstreams(SELECTOR, Collections.emptyList());
        DefaultConnectionConfigProvider provider =
                new DefaultConnectionConfigProvider("random", SELECTOR);

        ShenyuException exception =
                assertThrows(ShenyuException.class, () -> provider.getProxiedService("127.0.0.1"));

        assertTrue(exception.getMessage().contains("don't have any upstream"));
    }
}
