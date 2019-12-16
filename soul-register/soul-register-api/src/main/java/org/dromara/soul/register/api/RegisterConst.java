/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.api;

import org.dromara.soul.common.constant.Constants;

/**
 * RegisterConst
 *
 * @author sixh
 */
public class RegisterConst implements Constants {
    /**
     * The constant URL_CLUSTER_KEY.
     *
     * @see org.dromara.soul.register.api.config.RegistryConfig
     */
    public static final String URL_CLUSTER_KEY = "cluster";

    /**
     * The constant URL_SPLIT_SYMBOL_KEY.
     *
     * @see org.dromara.soul.register.api.config.RegistryConfig
     */
    public static final String URL_SPLIT_SYMBOL_KEY = ",";
    /**
     * The constant URL_SPLIT_SYMBOL_KEY.
     *
     * @see org.dromara.soul.common.http.URL
     */
    public static final String BASE_URL_PATH_KEY = "/";

    public static final String EPHEMERAL_KEY = "ephemeral";

    public static final String EVN_KEY = "env";
}
