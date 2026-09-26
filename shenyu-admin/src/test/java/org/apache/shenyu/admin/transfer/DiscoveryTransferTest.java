/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
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

import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * test case for {@link DiscoveryTransfer}.
 */
public final class DiscoveryTransferTest {

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
