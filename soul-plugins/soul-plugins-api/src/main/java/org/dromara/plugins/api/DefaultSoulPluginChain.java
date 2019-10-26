/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.plugins.api;

import org.dromara.plugins.api.dto.SoulRequest;
import org.dromara.plugins.api.dto.SoulResponse;

import java.util.List;

/**
 * The type Default soul plugin chain.
 *
 * @author xiaoyu
 */
public class DefaultSoulPluginChain implements SoulPluginChain {

    private int index;

    private final List<SoulPlugin> plugins;

    /**
     * Instantiates a new Default soul plugin chain.
     *
     * @param plugins the plugins
     */
    DefaultSoulPluginChain(final List<SoulPlugin> plugins) {
        this.plugins = plugins;
    }

    @Override
    public SoulResponse execute(SoulRequest soulRequest) {
        if (this.index < plugins.size()) {
            SoulPlugin plugin = plugins.get(this.index++);
            return plugin.execute(soulRequest, this);
        } else {
            return null;
        }
    }
}
