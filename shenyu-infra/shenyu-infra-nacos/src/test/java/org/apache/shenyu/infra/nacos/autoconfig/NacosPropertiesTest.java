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

package org.apache.shenyu.infra.nacos.autoconfig;

import org.apache.shenyu.infra.nacos.config.NacosConfig;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class NacosPropertiesTest {

    @Test
    void testGetNacos() {
        NacosProperties nacosProperties = new NacosProperties();
        NacosConfig defaultConfig = nacosProperties.getNacos();

        assertNotNull(defaultConfig, "Default NacosConfig should not be null");
    }

    @Test
    void testSetNacos() {
        NacosProperties nacosProperties = new NacosProperties();
        NacosConfig customConfig = NacosConfig.builder().build();

        nacosProperties.setNacos(customConfig);

        assertEquals(customConfig, nacosProperties.getNacos(), "NacosConfig should be set correctly");
    }
}
