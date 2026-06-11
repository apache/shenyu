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

package org.apache.shenyu.admin.model.event.instance;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class InstanceInfoReportEventTest {

    @Test
    void testBuilderAndGetters() {
        InstanceInfoReportEvent event = InstanceInfoReportEvent.builder()
                .instanceIp("127.0.0.1")
                .instancePort("8080")
                .instanceType("grpc")
                .instanceInfo("info")
                .namespaceId("ns")
                .instanceState(1)
                .build();
        assertNotNull(event);
        assertEquals("127.0.0.1", event.getInstanceIp());
        assertEquals("8080", event.getInstancePort());
        assertEquals("grpc", event.getInstanceType());
        assertEquals("info", event.getInstanceInfo());
        assertEquals("ns", event.getNamespaceId());
        assertEquals(1, event.getInstanceState());
    }
}
