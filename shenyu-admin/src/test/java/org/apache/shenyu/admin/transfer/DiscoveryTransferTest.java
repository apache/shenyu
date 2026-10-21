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

import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for discovery namespace mapping and upstream properties.
 */
public final class DiscoveryTransferTest {

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

    @Test
    public void testMapToDTOKeepsNamespaceId() {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setId("1");
        discoveryDO.setDiscoveryName("prod-registry");
        discoveryDO.setDiscoveryType("zookeeper");
        discoveryDO.setNamespaceId("ns-1");

        DiscoveryDTO discoveryDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryDO);

        assertEquals("ns-1", discoveryDTO.getNamespaceId());
        assertEquals("prod-registry", discoveryDTO.getName());
        assertEquals("zookeeper", discoveryDTO.getType());
    }

    @Test
    public void testMapToDTOWithoutNamespaceId() {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setId("1");
        discoveryDO.setDiscoveryName("prod-registry");
        discoveryDO.setDiscoveryType("zookeeper");

        DiscoveryDTO discoveryDTO = DiscoveryTransfer.INSTANCE.mapToDTO(discoveryDO);

        assertNull(discoveryDTO.getNamespaceId());
    }

    private DiscoveryUpstreamData upstream(final String props) {
        DiscoveryUpstreamData data = new DiscoveryUpstreamData();
        data.setProtocol("http://");
        data.setUrl("127.0.0.1:18882");
        data.setDateCreated(new Timestamp(0));
        data.setProps(props);
        return data;
    }

    @Test
    public void testMapToVoKeepsDiscoveryNameAndTypeSeparate() {
        DiscoveryDO discoveryDO = DiscoveryDO.builder()
                .id("id1")
                .discoveryName("prod-registry")
                .discoveryType("zookeeper")
                .discoveryLevel("0")
                .serverList("127.0.0.1:2181")
                .pluginName("divide")
                .props("{}")
                .namespaceId("6495bdf1d3c84a1c9c65ee88e927c19c")
                .build();
        DiscoveryVO discoveryVO = DiscoveryTransfer.INSTANCE.mapToVo(discoveryDO);
        assertNotNull(discoveryVO);
        assertEquals("prod-registry", discoveryVO.getDiscoveryName());
        assertEquals("zookeeper", discoveryVO.getDiscoveryType());
        assertEquals("id1", discoveryVO.getId());
        assertEquals("127.0.0.1:2181", discoveryVO.getServerList());
        assertEquals("divide", discoveryVO.getPluginName());
    }
}
