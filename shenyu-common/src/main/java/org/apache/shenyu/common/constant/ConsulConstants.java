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
 * Consul constant.
 */
public final class ConsulConstants implements Constants {

    /**
     * default value of config version index.
     */
    public static final Long INIT_CONFIG_VERSION_INDEX = -1L;

    /**
     * buildConsulSyncPrefix.
     *
     * @return /shenyu/instanceName/sync
     */
    public static String buildConsulSyncPrefix() {
        return String.join(SEPARATOR, InstanceConstants.getShenyuPrefixPath(), "sync");
    }

    /**
     * buildConsulPluginData.
     *
     * @return /shenyu/instanceName/sync/plugin
     */
    public static String buildConsulPluginData() {
        return String.join(SEPARATOR, buildConsulSyncPrefix(), PLUGIN_DATA);
    }

    /**
     * buildConsulSelectorData.
     *
     * @return /shenyu/instanceName/sync/selector
     */
    public static String buildConsulSelectorData() {
        return String.join(SEPARATOR, buildConsulSyncPrefix(), SELECTOR_DATA);
    }

    /**
     * buildConsulRuleData.
     *
     * @return /shenyu/instanceName/sync/rule
     */
    public static String buildConsulRuleData() {
        return String.join(SEPARATOR, buildConsulSyncPrefix(), RULE_DATA);
    }

    /**
     * buildConsulAuthData.
     *
     * @return /shenyu/instanceName/sync/auth
     */
    public static String buildConsulAuthData() {
        return String.join(SEPARATOR, buildConsulSyncPrefix(), AUTH_DATA);
    }

    /**
     * buildConsulMetaData.
     *
     * @return /shenyu/instanceName/sync/metaData
     */
    public static String buildConsulMetaData() {
        return String.join(SEPARATOR, buildConsulSyncPrefix(), META_DATA);
    }
}
