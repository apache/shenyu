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

package org.apache.shenyu.agent.core.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.config.ShenyuAgentConfig;
import org.apache.shenyu.agent.core.enums.SingletonHolder;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The type Shenyu agent config utils.
 */
public final class ShenyuAgentConfigUtils {
    
    private ShenyuAgentConfigUtils() {
    }
    
    /**
     * Gets supports.
     *
     * @return the supports
     */
    public static Set<String> getSupports() {
        Map<String, List<String>> supports = getConfig().getSupports();
        return supports.values().stream().flatMap(Collection::stream).filter(StringUtils::isNoneEmpty).collect(Collectors.toSet());
    }
    
    /**
     * Gets plugin config map.
     *
     * @return the plugin config map
     */
    public static Map<String, AgentPluginConfig> getPluginConfigMap() {
        return getConfig().getPlugins().values().stream()
                .map(Map::entrySet).flatMap(Set::stream)
                .collect(Collectors.toMap(Entry::getKey, entry -> GsonUtils.getInstance()
                        .fromJson(GsonUtils.getInstance().toJson(entry.getValue()), AgentPluginConfig.class)));
    }
    
    /**
     * Gets config.
     *
     * @return the config
     */
    public static ShenyuAgentConfig getConfig() {
        return SingletonHolder.INSTANCE.get(ShenyuAgentConfig.class);
    }
    
    /**
     * Sets config.
     *
     * @param agentConfig the agent config
     */
    public static void setConfig(final ShenyuAgentConfig agentConfig) {
        SingletonHolder.INSTANCE.put(agentConfig);
    }
}
