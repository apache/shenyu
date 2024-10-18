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

/**
 * DefaultPathConstants.
 */
public final class DefaultPathConstants implements Constants {

    /**
     * The constant SELECTOR_JOIN_RULE.
     */
    public static final String SELECTOR_JOIN_RULE = "-";

    public static final String PRE_FIX = "/shenyu";

    /**
     * The constant PLUGIN_PARENT.
     */
    public static final String PLUGIN_PARENT = PRE_FIX + "/plugin";

    /**
     * The constant SELECTOR_PARENT.
     */
    public static final String SELECTOR_PARENT = PRE_FIX + "/selector";

    public static final String RULE_PARENT = PRE_FIX + "/rule";

    /**
     * The constant APP_AUTH_PARENT.
     */
    public static final String APP_AUTH_PARENT = PRE_FIX + "/auth";

    /**
     * The constant META_DATA.
     */
    public static final String META_DATA = PRE_FIX + "/metaData";

    /**
     * The constant PROXY_SELECTOR.
     */
    public static final String PROXY_SELECTOR = PRE_FIX + "/proxySelectorData";

    /**
     * The constant DISCOVERY_UPSTREAM.
     */
    public static final String DISCOVERY_UPSTREAM = PRE_FIX + "/discoveryUpstream";

    /**
     * acquire app_auth_path.
     *
     * @param namespaceId namespaceId
     * @param appKey appKey
     * @return app_auth_path string
     */
    public static String buildAppAuthPath(final String namespaceId, final String appKey) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, APP_AUTH_PARENT, appKey));
    }

    /**
     * Build meta data path string.
     *
     * @param namespaceId the namespace id
     * @param path the path
     * @return the string
     */
    public static String buildMetaDataPath(final String namespaceId, final String path) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, META_DATA, path));
    }

    /**
     * buildPluginParentPath.
     *
     * @return zk path for plugin
     */
    public static String buildPluginParentPath() {
        return handlePathData(String.join(PATH_SEPARATOR, PLUGIN_PARENT).replaceAll("//", PATH_SEPARATOR));
    }

    /**
     * buildPluginRealPath.
     *
     * @param namespaceId namespaceId
     * @param pluginName pluginName
     * @return zk path for plugin
     */
    public static String buildPluginPath(final String namespaceId, final String pluginName) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, PLUGIN_PARENT, pluginName));
    }

    /**
     * buildSelectorParentPath.
     *
     * @param namespaceId namespaceId
     * @param pluginName pluginName
     * @return zk path for selector
     */
    public static String buildSelectorParentPath(final String namespaceId, final String pluginName) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, SELECTOR_PARENT, pluginName));
    }

    /**
     * buildSelectorRealPath.
     *
     * @param namespaceId namespaceId
     * @param pluginName pluginName
     * @param selectorId selectorId
     * @return zk full path for selector
     */
    public static String buildSelectorRealPath(final String namespaceId, final String pluginName, final String selectorId) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, SELECTOR_PARENT, pluginName, selectorId));
    }

    /**
     * buildRuleParentPath.
     *
     * @param namespaceId namespaceId
     * @param pluginName pluginName
     * @return zk rule parent path.
     */
    public static String buildRuleParentPath(final String namespaceId, final String pluginName) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, RULE_PARENT, pluginName));
    }

    /**
     * buildRulePath.
     *
     * @param namespaceId namespaceId
     * @param pluginName pluginName
     * @param selectorId selectorId
     * @param ruleId     ruleId
     * @return /shenyu/rule/pluginName/selectorId-ruleId
     */
    public static String buildRulePath(final String namespaceId, final String pluginName, final String selectorId, final String ruleId) {
        return handlePathData(String.join(PATH_SEPARATOR, buildRuleParentPath(PATH_SEPARATOR + namespaceId, pluginName), String.join(SELECTOR_JOIN_RULE, selectorId, ruleId)));
    }

    /**
     * buildProxySelector.
     *
     * @param namespaceId       namespaceId
     * @param pluginName        pluginName
     * @param proxySelectorName selectorId
     * @return /namespaceId/shenyu/proxySelectorData/pluginName/proxySelectorName
     */
    public static String buildProxySelectorPath(final String namespaceId, final String pluginName, final String proxySelectorName) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, PROXY_SELECTOR, pluginName, proxySelectorName));
    }

    /**
     * buildDiscoveryUpstreamPath.
     *
     * @param namespaceId       namespaceId
     * @param pluginName        pluginName
     * @param proxySelectorName selectorId
     * @return /namespaceId/shenyu/discoveryUpstream/pluginName/proxySelectorName
     */
    public static String buildDiscoveryUpstreamPath(final String namespaceId, final String pluginName, final String proxySelectorName) {
        return handlePathData(String.join(PATH_SEPARATOR, PATH_SEPARATOR + namespaceId, DISCOVERY_UPSTREAM, pluginName, proxySelectorName));
    }
    
    /**
     * replace // to /.
     *
     * @param path path
     * @return path
     */
    public static String handlePathData(final String path) {
        return path.replaceAll("//", PATH_SEPARATOR);
    }
}
