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
import org.junit.jupiter.api.Test;

import static org.apache.shenyu.common.constant.Constants.PATH_SEPARATOR;
import static org.apache.shenyu.common.constant.DefaultPathConstants.handlePathData;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;

/**
 * Test cases for ZkPathConstants.
 */
public final class DefaultPathConstantsTest {

    private static final String APP_AUTH_PARENT = DefaultPathConstants.PRE_FIX + "/auth";

    private static final String META_DATA_PARENT = DefaultPathConstants.PRE_FIX + "/metaData";

    private static final String PLUGIN_PARENT = DefaultPathConstants.PRE_FIX + "/plugin";

    private static final String SELECTOR_PARENT = DefaultPathConstants.PRE_FIX + "/selector";

    private static final String RULE_PARENT = DefaultPathConstants.PRE_FIX + "/rule";

    private static final String SELECTOR_JOIN_RULE = "-";

    private static final String SEPARATOR = "/";

    @Test
    public void testBuildAppAuthPath() {
        String appKey = RandomStringUtils.randomAlphanumeric(10);
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(Constants.SYS_DEFAULT_NAMESPACE_ID, appKey);
        assertThat(appAuthPath, notNullValue());
        assertThat(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, APP_AUTH_PARENT, appKey).replaceAll("//", PATH_SEPARATOR), equalTo(appAuthPath));
    }

    @Test
    public void testBuildMetaDataPath() {
        String metadata = RandomStringUtils.randomAlphanumeric(10);
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(Constants.SYS_DEFAULT_NAMESPACE_ID, metadata);
        assertThat(metaDataPath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, META_DATA_PARENT, metadata)), equalTo(metaDataPath));
    }

    @Test
    public void testBuildPluginParentPath() {
        String pluginParentPath = DefaultPathConstants.buildPluginParentPath();
        assertThat(pluginParentPath, notNullValue());
        assertThat(PLUGIN_PARENT, equalTo(pluginParentPath));
    }

    @Test
    public void testBuildPluginPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String pluginPath = DefaultPathConstants.buildPluginPath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName);
        assertThat(pluginPath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, PLUGIN_PARENT, pluginName)), equalTo(pluginPath));
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, DefaultPathConstants.buildPluginParentPath(), pluginName)), equalTo(pluginPath));
    }

    @Test
    public void testBuildSelectorParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorParentPath = handlePathData(DefaultPathConstants.buildSelectorParentPath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName));
        assertThat(selectorParentPath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, SELECTOR_PARENT, pluginName)), equalTo(selectorParentPath));
    }

    @Test
    public void testBuildSelectorRealPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        String selectorRealPath = handlePathData(DefaultPathConstants.buildSelectorRealPath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName, selectorId));
        assertThat(selectorRealPath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, SELECTOR_PARENT, pluginName, selectorId)), equalTo(selectorRealPath));
    }

    @Test
    public void testBuildRuleParentPath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String ruleParentPath = handlePathData(DefaultPathConstants.buildRuleParentPath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName));
        assertThat(ruleParentPath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, RULE_PARENT, pluginName)), equalTo(ruleParentPath));
    }

    @Test
    public void testBuildRulePath() {
        String pluginName = RandomStringUtils.randomAlphanumeric(10);
        String selectorId = RandomStringUtils.randomAlphanumeric(10);
        String ruleId = RandomStringUtils.randomAlphanumeric(10);
        String rulePath = handlePathData(DefaultPathConstants.buildRulePath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName, selectorId, ruleId));
        assertThat(rulePath, notNullValue());
        assertThat(handlePathData(String.join(SEPARATOR, PATH_SEPARATOR + Constants.SYS_DEFAULT_NAMESPACE_ID, RULE_PARENT, pluginName,
                String.join(SELECTOR_JOIN_RULE, selectorId, ruleId))), equalTo(rulePath));
        assertThat(handlePathData(String.join(SEPARATOR,
                handlePathData(DefaultPathConstants.buildRuleParentPath(Constants.SYS_DEFAULT_NAMESPACE_ID, pluginName)),
                String.join(SELECTOR_JOIN_RULE, selectorId, ruleId))), equalTo(rulePath));
    }
}
