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

package org.apache.shenyu.admin.config.properties;

import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.comparesEqualTo;

/**
 * Test cases for HttpSyncProperties.
 */
@ExtendWith(MockitoExtension.class)
public final class HttpSyncPropertiesTest extends AbstractConfigurationTest {

    @Test
    public void testDefault() {
        load(HttpSyncPropertiesConfiguration.class);
        HttpSyncProperties httpSyncProperties = getContext().getBean(HttpSyncProperties.class);
        httpSyncProperties.setNotifyBatchSize(0);
        assertThat(httpSyncProperties.isEnabled(), comparesEqualTo(true));
        Assertions.assertEquals(httpSyncProperties.getNotifyBatchSize(), 0);
        assertThat(httpSyncProperties.getRefreshInterval(), comparesEqualTo(Duration.ofMinutes(5)));
        Assertions.assertEquals(Duration.ZERO, httpSyncProperties.getCacheRebuildWindow());
        Assertions.assertEquals(1024, httpSyncProperties.getCacheRebuildMaxPendingKeys());
    }

    @Test
    public void testSpecified() {
        load(HttpSyncPropertiesConfiguration.class, "shenyu.sync.http.enabled=false", "shenyu.sync.http.refreshInterval=1m");
        HttpSyncProperties httpSyncProperties = getContext().getBean(HttpSyncProperties.class);
        assertThat(httpSyncProperties.isEnabled(), comparesEqualTo(false));
        assertThat(httpSyncProperties.getRefreshInterval(), comparesEqualTo(Duration.ofMinutes(1)));
    }

    @Test
    void testCoalescingConfiguration() {
        load(HttpSyncPropertiesConfiguration.class, "shenyu.sync.http.cache-rebuild-window=10ms", "shenyu.sync.http.cache-rebuild-max-pending-keys=32");
        HttpSyncProperties properties = getContext().getBean(HttpSyncProperties.class);
        Assertions.assertEquals(Duration.ofMillis(10), properties.getCacheRebuildWindow());
        Assertions.assertEquals(32, properties.getCacheRebuildMaxPendingKeys());
        Assertions.assertThrows(IllegalArgumentException.class, () -> properties.setCacheRebuildWindow(Duration.ofMillis(-1)));
        Assertions.assertThrows(IllegalArgumentException.class, () -> properties.setCacheRebuildWindow(Duration.ofSeconds(2)));
        Assertions.assertThrows(IllegalArgumentException.class, () -> properties.setCacheRebuildWindow(Duration.ofNanos(1)));
        Assertions.assertThrows(IllegalArgumentException.class, () -> properties.setCacheRebuildWindow(null));
        Assertions.assertThrows(IllegalArgumentException.class, () -> properties.setCacheRebuildMaxPendingKeys(0));
    }

    @Configuration
    @EnableConfigurationProperties(HttpSyncProperties.class)
    static class HttpSyncPropertiesConfiguration {
    }
}
