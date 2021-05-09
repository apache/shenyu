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
 * RedisKeyConstants.
 */
public final class RedisKeyConstants implements Constants {

    /**
     * The constant PLUGIN.
     */
    public static final String PLUGIN = "plugin";

    /**
     * The constant SELECTOR.
     */
    public static final String SELECTOR = "selector";

    /**
     * The constant RULE.
     */
    public static final String RULE = "rule";

    private static final String PLUGIN_INFO = ":info";

    private static final String PLUGIN_SELECTOR = ":selector";

    /**
     * this is a function.
     *
     * @param pluginName pluginName
     * @return java.lang.String string
     */
    public static String pluginInfoKey(final String pluginName) {
        return String.join("", pluginName, PLUGIN_INFO);

    }

    /**
     * this is a function.
     *
     * @param pluginName pluginName
     * @return java.lang.String string
     */
    public static String pluginSelectorKey(final String pluginName) {
        return String.join("", pluginName, PLUGIN_SELECTOR);

    }

}
