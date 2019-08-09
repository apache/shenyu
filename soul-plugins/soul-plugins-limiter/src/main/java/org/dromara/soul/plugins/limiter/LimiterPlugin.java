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

package org.dromara.soul.plugins.limiter;

import org.dromara.plugins.api.AbstractSoulPlugin;
import org.dromara.plugins.api.SoulPluginChain;
import org.dromara.soul.common.dto.SoulRequest;
import org.dromara.soul.common.dto.SoulResponse;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;

/**
 * @author xiaoyu(Myth)
 */
public class LimiterPlugin extends AbstractSoulPlugin {


    @Override
    protected SoulResponse doExecute(SoulRequest soulRequest, SoulPluginChain chain) {
        return null;
    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.RATE_LIMITER.getOrder();
    }

    @Override
    public String named() {
        return PluginEnum.RATE_LIMITER.getName();
    }
}
