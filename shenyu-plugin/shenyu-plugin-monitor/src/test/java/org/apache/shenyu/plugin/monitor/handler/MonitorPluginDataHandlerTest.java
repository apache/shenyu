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

package org.apache.shenyu.plugin.monitor.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.metrics.facade.MetricsTrackerFacade;
import org.junit.AfterClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Test case for MonitorPluginHandler.
 */
public final class MonitorPluginDataHandlerTest {

    private MonitorPluginDataHandler monitorPluginDataHandler = new MonitorPluginDataHandler();

    @Test
    public void testHandlerPlugin() {
        //test with plugin enable: false
        String config = "{\"host\":\"localhost\",\"port\":\"19996\",\"async\":false,\"metricsName\":\"prometheus\"}";
        PluginData pluginData = new PluginData("", "", config, "1", false);
        monitorPluginDataHandler.handlerPlugin(pluginData);
        assertFalse(MetricsTrackerFacade.getInstance().isStarted());

        //test with plugin config incomplete
        config = "{\"host\":\"localhost\",\"async\":false,\"metricsName\":\"prometheus\"}";
        pluginData = new PluginData("", "", config, "1", true);
        monitorPluginDataHandler.handlerPlugin(pluginData);
        assertFalse(MetricsTrackerFacade.getInstance().isStarted());

        //test with plugin config complete
        config = "{\"host\":\"localhost\",\"port\":\"19997\",\"async\":false,\"metricsName\":\"prometheus\"}";
        pluginData = new PluginData("", "", config, "1", true);
        monitorPluginDataHandler.handlerPlugin(pluginData);
        assertTrue(MetricsTrackerFacade.getInstance().isStarted());

        //test with plugin config modified
        config = "{\"host\":\"localhost\",\"port\":\"19998\",\"async\":true,\"metricsName\":\"prometheus\"}";
        pluginData = new PluginData("", "", config, "1", true);
        monitorPluginDataHandler.handlerPlugin(pluginData);
        assertTrue(MetricsTrackerFacade.getInstance().isStarted());
    }

    @Test
    public void testPluginNamed() {
        assertEquals(PluginEnum.MONITOR.getName(), monitorPluginDataHandler.pluginNamed());
    }
    
    @AfterClass
    public static void close() {
        MetricsTrackerFacade.getInstance().stop();
    }
}
