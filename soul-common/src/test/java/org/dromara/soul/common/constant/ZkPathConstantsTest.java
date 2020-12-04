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

package org.dromara.soul.common.constant;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;


/**
 * Test cases for ZkPathConstants.
 *
 * @author songyuequan
 */
public final class ZkPathConstantsTest {

    public static final String APP_AUTH_PARENT = "/soul/auth";

    public static final String META_DATA_PARENT = "/soul/metaData";

    public static final String PLUGIN_PARENT = "/soul/plugin";

    public static final String SELECTOR_PARENT = "/soul/selector";

    public static final String RULE_PARENT = "/soul/rule";

    public static final String SELECTOR_JOIN_RULE = "-";

    @Test
    public void testBuildAppAuthPath() {
        String appKey = RandomStringUtils.randomAlphanumeric(10);
        assertNotNull(ZkPathConstants.buildAppAuthPath(appKey));
        assertEquals(String.join("/", APP_AUTH_PARENT, appKey), ZkPathConstants.buildAppAuthPath(appKey));
    }

    @Test
    public void testBuildMetaDataPath() {
        String metadata = RandomStringUtils.randomAlphanumeric(10);
        assertNotNull(ZkPathConstants.buildMetaDataPath(metadata));
        assertEquals(String.join("/", META_DATA_PARENT, metadata), ZkPathConstants.buildMetaDataPath(metadata));
    }

    @Test
    public void testBuildPluginParentPath() {
        assertEquals(PLUGIN_PARENT, ZkPathConstants.buildPluginParentPath());
    }

    @Test
    public void testBuildPluginPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        assertEquals(String.join("/", PLUGIN_PARENT, pluginName), ZkPathConstants.buildPluginPath(pluginName));
        assertEquals(String.join("/", ZkPathConstants.buildPluginParentPath(), pluginName), ZkPathConstants.buildPluginPath(pluginName));
    }

    @Test
    public void testBuildSelectorParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        assertEquals(String.join("/", SELECTOR_PARENT, pluginName), ZkPathConstants.buildSelectorParentPath(pluginName));
    }

    @Test
    public void testBuildSelectorRealPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        assertEquals(String.join("/", SELECTOR_PARENT, pluginName, selectorId), ZkPathConstants.buildSelectorRealPath(pluginName, selectorId));
    }

    @Test
    public void testBuildRuleParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        assertEquals(String.join("/", RULE_PARENT, pluginName), ZkPathConstants.buildRuleParentPath(pluginName));
    }

    @Test
    public void testBuildRulePath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        String ruleId = RandomStringUtils.randomAlphanumeric(10);
        assertEquals(String.join("/", RULE_PARENT, pluginName, selectorId + SELECTOR_JOIN_RULE + ruleId), ZkPathConstants.buildRulePath(pluginName, selectorId, ruleId));
        assertEquals(String.join("/", ZkPathConstants.buildRuleParentPath(pluginName), selectorId + SELECTOR_JOIN_RULE + ruleId), ZkPathConstants.buildRulePath(pluginName, selectorId, ruleId));
    }
}
