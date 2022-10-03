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

package org.apache.shenyu.plugin.casdoor.config;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CasdoorConfigTest {

    @Test
    public void casdoorConfig() {
        CasdoorConfig casdoorConfig = new CasdoorConfig("a", "b", "c", "d", "e", "f");
        assertEquals("a", casdoorConfig.getEndpoint());
        assertEquals("b", casdoorConfig.getClientId());
        assertEquals("c", casdoorConfig.getClientSecret());
        assertEquals("d", casdoorConfig.getCertificate());
        assertEquals("e", casdoorConfig.getOrganizationName());
        assertEquals("f", casdoorConfig.getApplicationName());

        CasdoorConfig casdoorConfig1 = new CasdoorConfig();
        casdoorConfig1.setEndpoint("a");
        casdoorConfig1.setClientId("b");
        casdoorConfig1.setClientSecret("c");
        casdoorConfig1.setCertificate("d");
        casdoorConfig1.setOrganizationName("e");
        casdoorConfig1.setApplicationName("f");
        assertEquals("a", casdoorConfig1.getEndpoint());
        assertEquals("b", casdoorConfig1.getClientId());
        assertEquals("c", casdoorConfig1.getClientSecret());
        assertEquals("d", casdoorConfig1.getCertificate());
        assertEquals("e", casdoorConfig1.getOrganizationName());
        assertEquals("f", casdoorConfig1.getApplicationName());
        assertEquals("authConfig{endpoint='a', clientId='b', clientSecret='c', certificate='d', organizationName='e', applicationName='f'}", casdoorConfig1.toString());
    }
}
