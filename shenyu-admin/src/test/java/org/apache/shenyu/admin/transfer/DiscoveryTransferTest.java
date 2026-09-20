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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DiscoveryTransferTest {

    @Test
    void shouldAcceptNestedLabelsWithoutChangingProps() {
        DiscoveryUpstreamData data = upstream("{\"labels\":{\"release\":\"canary\"},\"custom\":{\"tls\":true},\"warmupTime\":10,\"gray\":\"false\"}");
        String original = data.getProps();
        CommonUpstream result = DiscoveryTransfer.INSTANCE.mapToCommonUpstream(data);
        assertTrue(result.isHealthCheckEnabled());
        assertEquals("127.0.0.1:18882", result.getUpstreamUrl());
        assertEquals(original, data.getProps());
    }

    @Test
    void shouldPreserveBooleanAndStringHealthCheckSettings() {
        for (String value : new String[]{"false", "\"false\""}) {
            assertFalse(DiscoveryTransfer.INSTANCE.mapToCommonUpstream(upstream(
                    "{\"labels\":{\"release\":\"canary\"},\"healthCheckEnabled\":" + value + "}")).isHealthCheckEnabled());
        }
        for (String value : new String[]{"true", "\"true\""}) {
            assertTrue(DiscoveryTransfer.INSTANCE.mapToCommonUpstream(upstream(
                    "{\"healthCheckEnabled\":" + value + "}")).isHealthCheckEnabled());
        }
    }

    @Test
    void shouldKeepLegacyDefaults() {
        for (String props : new String[]{null, "{}", "null", "{\"labels\":{}}", "{\"healthCheckEnabled\":null}"}) {
            assertTrue(DiscoveryTransfer.INSTANCE.mapToCommonUpstream(upstream(props)).isHealthCheckEnabled());
        }
        assertNull(DiscoveryTransfer.INSTANCE.mapToCommonUpstream(null));
    }

    private DiscoveryUpstreamData upstream(final String props) {
        DiscoveryUpstreamData data = new DiscoveryUpstreamData();
        data.setProtocol("http://");
        data.setUrl("127.0.0.1:18882");
        data.setDateCreated(new Timestamp(0));
        data.setProps(props);
        return data;
    }
}
