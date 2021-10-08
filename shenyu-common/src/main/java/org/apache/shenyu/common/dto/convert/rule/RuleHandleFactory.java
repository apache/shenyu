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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.SofaRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The RuleHandle factory.
 */
public final class RuleHandleFactory {

    /**
     * The RpcType to RuleHandle class map.
     */
    private static final Map<String, Class<? extends RuleHandle>> RPC_TYPE_TO_RULE_HANDLE_CLASS = new ConcurrentHashMap<>();

    /**
     * The default RuleHandle.
     */
    private static final Class<? extends RuleHandle> DEFAULT_RULE_HANDLE = SpringCloudRuleHandle.class;

    static {
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(PluginEnum.DIVIDE.getName(), DivideRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(PluginEnum.DUBBO.getName(), DubboRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(PluginEnum.SOFA.getName(), SofaRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(PluginEnum.SPRING_CLOUD.getName(), SpringCloudRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(PluginEnum.CONTEXT_PATH.getName(), ContextMappingHandle.class);
    }

    /**
     * Get a RuleHandle object with given rpc type and path.
     * @param pluginName plugin's name.
     * @param path       path.
     * @param rpcExt     rpc ext.
     * @return           RuleHandle object.
     */
    public static RuleHandle ruleHandle(final String pluginName, final String path, final String rpcExt) {
        Class<? extends RuleHandle> clazz = RPC_TYPE_TO_RULE_HANDLE_CLASS.getOrDefault(pluginName, DEFAULT_RULE_HANDLE);
        try {
            return clazz.newInstance().createDefault(path, rpcExt);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new ShenyuException(
                    String.format("Init RuleHandle failed with plugin name: %s, rule class: %s, exception: %s",
                            pluginName,
                            clazz.getSimpleName(),
                            e.getMessage()));
        }
    }
}
