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

package org.dromara.soul.common.dto.convert.rule;

import org.dromara.soul.common.dto.convert.rule.impl.DivideRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.SofaRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.exception.SoulException;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The RuleHandle factory.
 *
 * @author yiwenlong (wlong.yi@gmail.com)
 */
public final class RuleHandleFactory {

    /**
     * The RpcType to RuleHandle class map.
     */
    private static final Map<RpcTypeEnum, Class<? extends RuleHandle>> RPC_TYPE_TO_RULE_HANDLE_CLASS = new ConcurrentHashMap<>();

    /**
     * The default RuleHandle.
     */
    private static final Class<? extends RuleHandle> DEFAULT_RULE_HANDLE = SpringCloudRuleHandle.class;

    static {
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(RpcTypeEnum.HTTP, DivideRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(RpcTypeEnum.DUBBO, DubboRuleHandle.class);
        RPC_TYPE_TO_RULE_HANDLE_CLASS.put(RpcTypeEnum.SOFA, SofaRuleHandle.class);
    }

    /**
     * Get a RuleHandle object with given rpc type and path.
     * @param rpcType   rpc type.
     * @param path      path.
     * @return          RuleHandle object.
     */
    public static RuleHandle ruleHandle(final RpcTypeEnum rpcType, final String path) {
        if (Objects.isNull(rpcType)) {
            return null;
        }
        Class<? extends RuleHandle> clazz = RPC_TYPE_TO_RULE_HANDLE_CLASS.getOrDefault(rpcType, DEFAULT_RULE_HANDLE);
        try {
            return clazz.newInstance().createDefault(path);
        } catch (InstantiationException | IllegalAccessException e) {
            throw new SoulException(
                    String.format("Init RuleHandle failed with rpc type: %s, rule class: %s, exception: %s",
                            rpcType,
                            clazz.getSimpleName(),
                            e.getMessage()));
        }
    }
}
