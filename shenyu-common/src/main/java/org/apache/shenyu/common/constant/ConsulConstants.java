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
 * Consul constants.
 */
public final class ConsulConstants {

    public static final String SYNC_PRE_FIX = "shenyu/sync";

    /**
     * plugin data path.
     */
    public static final String PLUGIN_DATA = SYNC_PRE_FIX + "/plugin";

    /**
     * selector data path.
     */
    public static final String SELECTOR_DATA = SYNC_PRE_FIX + "/selector";

    /**
     * rule data path.
     */
    public static final String RULE_DATA = SYNC_PRE_FIX + "/rule";

    /**
     * auth data path.
     */
    public static final String AUTH_DATA = SYNC_PRE_FIX + "/auth";

    /**
     * meta data path.
     */
    public static final String META_DATA = SYNC_PRE_FIX + "/meta";

    /**
     * default value of get config.
     */
    public static final String EMPTY_CONFIG_DEFAULT_VALUE = "{}";

    /**
     * default value of config version index.
     */
    public static final Long INIT_CONFIG_VERSION_INDEX = -1L;
}
