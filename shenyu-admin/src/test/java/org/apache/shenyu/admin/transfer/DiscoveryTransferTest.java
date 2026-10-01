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
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for DiscoveryTransfer.
 */
public final class DiscoveryTransferTest {

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

        assertEquals(null, discoveryDTO.getNamespaceId());
    }
}
