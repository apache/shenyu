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

package org.dromara.soul.common.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.dromara.soul.common.dto.convert.rule.DivideRuleHandle;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.rule.SofaRuleHandle;
import org.dromara.soul.common.dto.convert.rule.SpringCloudRuleHandle;
import org.dromara.soul.common.exception.SoulException;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * RpcTypeEnum.
 *
 * @author xiaoyu(549477611 @ qq.com)
 */
@RequiredArgsConstructor
@Getter
public enum RpcTypeEnum {

    /**
     * Http rpc type enum.
     */
    HTTP("http", true) {

        @Override
        public Serializable ruleHandle(final String path) {
            DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
            divideRuleHandle.setLoadBalance(getDefaultLoadBalance().getName());
            divideRuleHandle.setRetry(getDefaultRetry());
            return divideRuleHandle;
        }
    },

    /**
     * Dubbo rpc type enum.
     */
    DUBBO("dubbo", true) {

        @Override
        public Serializable ruleHandle(final String path) {
            DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
            dubboRuleHandle.setLoadBalance(getDefaultLoadBalance().getName());
            dubboRuleHandle.setRetries(getDefaultRetries());
            dubboRuleHandle.setTimeout(getDefaultTimeout());
            return dubboRuleHandle;
        }
    },

    /**
     * Sofa rpc type enum.
     */
    SOFA("sofa", true) {

        @Override
        public Serializable ruleHandle(final String path) {
            SofaRuleHandle sofaRuleHandle = new SofaRuleHandle();
            sofaRuleHandle.setLoadBalance(getDefaultLoadBalance().getName());
            sofaRuleHandle.setRetries(getDefaultRetries());
            sofaRuleHandle.setTimeout(getDefaultTimeout());
            return sofaRuleHandle;
        }
    },

    /**
     * Web socket rpc type enum.
     */
    WEB_SOCKET("websocket", true),

    /**
     * springCloud rpc type enum.
     */
    SPRING_CLOUD("springCloud", true),

    /**
     * motan.
     */
    MOTAN("motan", false),

    /**
     * grpc.
     */
    GRPC("grpc", false);


    private final String name;

    private final Boolean support;

    // some default values for rule handlers.
    private final LoadBalanceEnum defaultLoadBalance = LoadBalanceEnum.RANDOM;
    private final int defaultRetries = 0;
    private final long defaultTimeout = 3000;
    private final int defaultRetry = 0;
    /**
     * acquire operator supports.
     *
     * @return operator support.
     */
    public static List<RpcTypeEnum> acquireSupports() {
        return Arrays.stream(RpcTypeEnum.values())
                .filter(e -> e.support).collect(Collectors.toList());
    }

    /**
     * acquireByName.
     *
     * @param name this is rpc type
     * @return RpcTypeEnum rpc type enum
     */
    public static RpcTypeEnum acquireByName(final String name) {
        return Arrays.stream(RpcTypeEnum.values())
                .filter(e -> e.support && e.name.equals(name)).findFirst()
                .orElseThrow(() -> new SoulException(String.format(" this rpc type can not support %s", name)));
    }

    /**
     * ruleHandle
     * This method is design for overwrite.
     * @param path this is access path
     * @return Default rpc rule handler.
     */
    public Serializable ruleHandle(final String path) {
        SpringCloudRuleHandle springCloudRuleHandle = new SpringCloudRuleHandle();
        springCloudRuleHandle.setPath(path);
        return springCloudRuleHandle;
    }
}
