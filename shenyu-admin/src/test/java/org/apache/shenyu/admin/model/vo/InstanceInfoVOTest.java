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

package org.apache.shenyu.admin.model.vo;

import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class InstanceInfoVOTest {

    @Test
    void testGettersSetters() {
        InstanceInfoVO vo = new InstanceInfoVO();
        vo.setInstanceIp("127.0.0.1");
        vo.setInstancePort("8080");
        vo.setInstanceType("grpc");
        vo.setInstanceInfo("info");
        vo.setNamespaceId("ns");
        vo.setInstanceState(1);
        vo.setLastHeartBeatTime(123L);
        Timestamp now = new Timestamp(System.currentTimeMillis());
        vo.setDateCreated(now);
        vo.setDateUpdated(now);

        assertEquals("127.0.0.1", vo.getInstanceIp());
        assertEquals("8080", vo.getInstancePort());
        assertEquals("grpc", vo.getInstanceType());
        assertEquals("info", vo.getInstanceInfo());
        assertEquals("ns", vo.getNamespaceId());
        assertEquals(1, vo.getInstanceState());
        assertEquals(123L, vo.getLastHeartBeatTime());
        assertNotNull(vo.getDateCreated());
        assertNotNull(vo.getDateUpdated());
    }
}
