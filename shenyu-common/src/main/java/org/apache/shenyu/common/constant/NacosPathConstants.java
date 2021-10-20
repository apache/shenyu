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
 * Nacos path constant.
 */
public final class NacosPathConstants implements Constants {

    /**
     * Nacos config default group.
     */
    public static final String GROUP = "DEFAULT_GROUP";

    /**
     * default time out of get config.
     */
    public static final long DEFAULT_TIME_OUT = 6000;

    private static final String JSON = "json";

    /**
     * buildNacosPluginData.
     *
     * @return shenyu.instanceName.plugin.json
     */
    public static String buildNacosPluginData() {
        return String.join(DOT_SEPARATOR, buildNacosPrefix(), PLUGIN_DATA, JSON);
    }

    /**
     * buildNacosSelectorData.
     *
     * @return shenyu.instanceName.selector.json
     */
    public static String buildNacosSelectorData() {
        return String.join(DOT_SEPARATOR, buildNacosPrefix(), SELECTOR_DATA, JSON);
    }

    /**
     * buildNacosRuleData.
     *
     * @return shenyu.instanceName.rule.json
     */
    public static String buildNacosRuleData() {
        return String.join(DOT_SEPARATOR, buildNacosPrefix(), RULE_DATA, JSON);
    }

    /**
     * buildNacosAuthData.
     *
     * @return shenyu.instanceName.auth.json
     */
    public static String buildNacosAuthData() {
        return String.join(DOT_SEPARATOR, buildNacosPrefix(), AUTH_DATA, JSON);
    }

    /**
     * buildNacosAuthData.
     *
     * @return shenyu.instanceName.metaData.json
     */
    public static String buildNacosMetaData() {
        return String.join(DOT_SEPARATOR, buildNacosPrefix(), META_DATA, JSON);
    }

    /**
     * buildNacosPrefix.
     *
     * @return shenyu.instanceName
     */
    private static String buildNacosPrefix() {
        return String.join(DOT_SEPARATOR, SHENYU, InstanceConstants.getInstanceName());
    }
}
