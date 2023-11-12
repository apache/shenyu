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

package org.apache.shenyu.admin.discovery;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class DiscoveryModeTest {

    @Test
    void enumValues() {
        DiscoveryMode[] values = DiscoveryMode.values();
        assertEquals(5, values.length);
        assertEquals(DiscoveryMode.LOCAL, values[0]);
        assertEquals(DiscoveryMode.ZOOKEEPER, values[1]);
        assertEquals(DiscoveryMode.NACOS, values[2]);
        assertEquals(DiscoveryMode.EUREKA, values[3]);
        assertEquals(DiscoveryMode.ETCD, values[4]);
    }
}
