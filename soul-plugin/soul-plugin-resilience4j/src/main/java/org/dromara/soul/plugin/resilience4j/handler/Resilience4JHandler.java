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

package org.dromara.soul.plugin.resilience4j.handler;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.plugin.resilience4j.factory.Resilience4JRegistryFactory;

/**
 * Resilience4J rule handle.
 *
 * @author zhanglei
 */
public class Resilience4JHandler implements PluginDataHandler {

    @Override
    public void handlerRule(final RuleData ruleData) {
        Resilience4JRegistryFactory.remove(getResourceName(ruleData));
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Resilience4JRegistryFactory.remove(getResourceName(ruleData));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.RESILIENCE4J.getName();
    }

    /**
     * Resource name.
     *
     * @param ruleData the ruleData
     * @return String
     */
    public static String getResourceName(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }
}
