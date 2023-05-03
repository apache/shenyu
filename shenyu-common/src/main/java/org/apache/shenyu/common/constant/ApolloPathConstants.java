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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * apollo path.
 */
public class ApolloPathConstants {

    /**
     * apollo config default group.
     */
    public static final String GROUP = "DEFAULT_GROUP";

    /**
     * plugin data id.
     */
    public static final String PLUGIN_DATA_ID = "shenyu.plugin.json";

    /**
     * selector data id.
     */
    public static final String SELECTOR_DATA_ID = "shenyu.selector.json";

    /**
     * rule data id.
     */
    public static final String RULE_DATA_ID = "shenyu.rule.json";

    /**
     * auth data id.
     */
    public static final String AUTH_DATA_ID = "shenyu.auth.json";

    /**
     * meta data id.
     */
    public static final String META_DATA_ID = "shenyu.meta.json";

    /**
     * register metadata id.
     */
    public static final String REGISTER_METADATA_ID = "shenyu.register.metadata";

    /**
     * register uri id.
     */
    public static final String REGISTER_URI_ID = "shenyu.register.uri";

    /**
     * default value of get config.
     */
    public static final String EMPTY_CONFIG_DEFAULT_VALUE = "{}";

    /**
     * default time out of get config.
     */
    public static final long DEFAULT_TIME_OUT = 6000;

    /**
     * get path key set.
     *
     * @return path key set
     */
    public static Set<String> pathKeySet() {
        return new HashSet<>(Arrays.asList(PLUGIN_DATA_ID, SELECTOR_DATA_ID, RULE_DATA_ID, AUTH_DATA_ID, META_DATA_ID));
    }
}

