/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.plugins.api;


import org.dromara.plugins.api.dto.SoulRequest;
import org.dromara.plugins.api.dto.SoulResponse;
import org.dromara.soul.common.enums.PluginTypeEnum;

/**
 * the soul plugin interface.
 *
 * @author xiaoyu
 */
public interface SoulPlugin {

    /**
     * Execute soul response.
     *
     * @param soulRequest the soul request
     * @param chain       the chain
     * @return the soul response
     */
    SoulResponse execute(SoulRequest soulRequest, SoulPluginChain chain);

    /**
     * return plugin type.
     * the plugin execution order
     * before type The first to perform then Function Type ,then last type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    PluginTypeEnum pluginType();

    /**
     * return plugin order .
     * This attribute To determine the plugin execution order in the same type plugin.
     *
     * @return int order
     */
    int getOrder();

    /**
     * acquire plugin name.
     * this is plugin name define  if you extend {@linkplain AbstractSoulPlugin } you must Provide the right name.
     * if you impl AbstractSoulPlugin this attribute not use.
     *
     * @return plugin name.
     */
    String named();

    /**
     * plugin is execute.
     * if return true this plugin can not execute.
     *
     * @param soulRequest the soul request
     * @return default false.
     */
    default Boolean skip(SoulRequest soulRequest) {
        return false;
    }

}

