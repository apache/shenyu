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

package org.apache.shenyu.common.constant;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Test;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for ZkPathConstants.
 *
 * @author songyuequan
 */
public final class ZkPathConstantsTest {

    private static final String APP_AUTH_PARENT = "/soul/auth";

    private static final String META_DATA_PARENT = "/soul/metaData";

    private static final String PLUGIN_PARENT = "/soul/plugin";

    private static final String SELECTOR_PARENT = "/soul/selector";

    private static final String RULE_PARENT = "/soul/rule";

    private static final String SELECTOR_JOIN_RULE = "-";

    private static final String SEPARATOR = "/";

    @Test
    public void testBuildAppAuthPath() {
        String appKey = RandomStringUtils.randomAlphanumeric(10);
        String appAuthPath = ZkPathConstants.buildAppAuthPath(appKey);
        assertThat(appAuthPath, notNullValue());
        assertThat(String.join(SEPARATOR, APP_AUTH_PARENT, appKey), equalTo(appAuthPath));
    }

    @Test
    public void testBuildMetaDataPath() {
        String metadata = RandomStringUtils.randomAlphanumeric(10);
        String metaDataPath = ZkPathConstants.buildMetaDataPath(metadata);
        assertThat(metaDataPath, notNullValue());
        assertThat(String.join(SEPARATOR, META_DATA_PARENT, metadata), equalTo(metaDataPath));
    }

    @Test
    public void testBuildPluginParentPath() {
        String pluginParentPath = ZkPathConstants.buildPluginParentPath();
        assertThat(pluginParentPath, notNullValue());
        assertThat(PLUGIN_PARENT, equalTo(pluginParentPath));
    }

    @Test
    public void testBuildPluginPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String pluginPath = ZkPathConstants.buildPluginPath(pluginName);
        assertThat(pluginPath, notNullValue());
        assertThat(String.join(SEPARATOR, PLUGIN_PARENT, pluginName), equalTo(pluginPath));
        assertThat(String.join(SEPARATOR, ZkPathConstants.buildPluginParentPath(), pluginName), equalTo(pluginPath));
    }

    @Test
    public void testBuildSelectorParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginName);
        assertThat(selectorParentPath, notNullValue());
        assertThat(String.join(SEPARATOR, SELECTOR_PARENT, pluginName), equalTo(selectorParentPath));
    }

    @Test
    public void testBuildSelectorRealPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        String selectorRealPath = ZkPathConstants.buildSelectorRealPath(pluginName, selectorId);
        assertThat(selectorRealPath, notNullValue());
        assertThat(String.join(SEPARATOR, SELECTOR_PARENT, pluginName, selectorId), equalTo(selectorRealPath));
    }

    @Test
    public void testBuildRuleParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginName);
        assertThat(ruleParentPath, notNullValue());
        assertThat(String.join(SEPARATOR, RULE_PARENT, pluginName), equalTo(ruleParentPath));
    }

    @Test
    public void testBuildRulePath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        String ruleId = RandomStringUtils.randomAlphanumeric(10);
        String rulePath = ZkPathConstants.buildRulePath(pluginName, selectorId, ruleId);
        assertThat(rulePath, notNullValue());
        assertThat(String.join(SEPARATOR, RULE_PARENT, pluginName, String.join(SELECTOR_JOIN_RULE, selectorId, ruleId)), equalTo(rulePath));
        assertThat(String.join(SEPARATOR, ZkPathConstants.buildRuleParentPath(pluginName), String.join(SELECTOR_JOIN_RULE, selectorId, ruleId)), equalTo(rulePath));
    }
}
