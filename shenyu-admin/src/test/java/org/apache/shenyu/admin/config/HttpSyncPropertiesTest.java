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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.junit.Assert.assertEquals;

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
        assertEquals(httpSyncProperties.getNotifyBatchSize(), 0);
        assertThat(httpSyncProperties.getRefreshInterval(), comparesEqualTo(Duration.ofMinutes(5)));
    }

    @Test
    public void testSpecified() {
        load(HttpSyncPropertiesConfiguration.class, "shenyu.sync.http.enabled=false", "shenyu.sync.http.refreshInterval=1m");
        HttpSyncProperties httpSyncProperties = getContext().getBean(HttpSyncProperties.class);
        assertThat(httpSyncProperties.isEnabled(), comparesEqualTo(false));
        assertThat(httpSyncProperties.getRefreshInterval(), comparesEqualTo(Duration.ofMinutes(1)));
    }

    @Configuration
    @EnableConfigurationProperties(HttpSyncProperties.class)
    static class HttpSyncPropertiesConfiguration {
    }
}
