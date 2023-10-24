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

package org.apache.shenyu.plugin.dubbo.common.cache;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * DubboConfigCacheTest test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DubboConfigCacheTest {

    @Test
    public void parserToDubboParamTest() {
        DubboConfigCache dubboConfigCache = new DubboConfigCache();
        String dubboJsonStr = "{\"group\":\"Group\",\"version\":\"2.6.5\",\"loadbalance\":\"random\",\"retries\":\"1\",\"timeout\":\"3000\",\"url\":\"http://192.168.55.113/dubbo\",\"sent\":\"true\",\"cluster\":\"failover\"}";
        DubboParam dubboParam = dubboConfigCache.parserToDubboParam(dubboJsonStr);
        assertNotNull(dubboParam);
        assertEquals(dubboParam.getCluster(), "failover");
        assertEquals(dubboParam.getTimeout(), 3000);
        assertEquals(dubboParam.getRetries(), 1);
        assertEquals(dubboParam.getUrl(), "http://192.168.55.113/dubbo");
        assertEquals(dubboParam.getVersion(), "2.6.5");
        assertEquals(dubboParam.getGroup(), "Group");
        assertEquals(dubboParam.getLoadbalance(), "random");
        assertEquals(dubboParam.getSent(), true);
    }

    @Test
    public void dubboParamTest() {
        DubboParam dubboParam = new DubboParam();
        dubboParam.setCluster("failover");
        dubboParam.setTimeout(3000);
        dubboParam.setRetries(1);
        dubboParam.setUrl("http://192.168.55.113/dubbo");
        dubboParam.setVersion("2.6.5");
        dubboParam.setGroup("Group");
        dubboParam.setSent(true);
        dubboParam.setLoadbalance("random");
        assertEquals(dubboParam.getCluster(), "failover");
        assertEquals(dubboParam.getTimeout(), 3000);
        assertEquals(dubboParam.getRetries(), 1);
        assertEquals(dubboParam.getUrl(), "http://192.168.55.113/dubbo");
        assertEquals(dubboParam.getVersion(), "2.6.5");
        assertEquals(dubboParam.getGroup(), "Group");
        assertEquals(dubboParam.getLoadbalance(), "random");
        assertEquals(dubboParam.getSent(), true);
    }
}
