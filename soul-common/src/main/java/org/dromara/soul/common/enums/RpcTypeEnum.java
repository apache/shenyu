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
import org.dromara.soul.common.utils.JsonUtils;

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

        private final LoadBalanceEnum   loadBalance = LoadBalanceEnum.RANDOM;
        private final int               retry = 0;

        @Override
        public Serializable ruleHandle(final String path) {
            DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
            divideRuleHandle.setLoadBalance(loadBalance.getName());
            divideRuleHandle.setRetry(retry);
            return divideRuleHandle;
        }
    },

    /**
     * Dubbo rpc type enum.
     */
    DUBBO("dubbo", true) {

        private final LoadBalanceEnum   loadBalance = LoadBalanceEnum.RANDOM;
        private final int               retries = 0;
        private final long              timeout = 3000;

        @Override
        public Serializable ruleHandle(final String path) {
            DubboRuleHandle dubboRuleHandle = new DubboRuleHandle();
            dubboRuleHandle.setLoadBalance(loadBalance.getName());
            dubboRuleHandle.setRetries(retries);
            dubboRuleHandle.setTimeout(timeout);
            return dubboRuleHandle;
        }
    },

    /**
     * Sofa rpc type enum.
     */
    SOFA("sofa", true) {

        private final LoadBalanceEnum   loadBalance = LoadBalanceEnum.RANDOM;
        private final int               retries = 0;
        private final long              timeout = 3000;

        @Override
        public Serializable ruleHandle(String path) {
            SofaRuleHandle sofaRuleHandle = new SofaRuleHandle();
            sofaRuleHandle.setLoadBalance(loadBalance.getName());
            sofaRuleHandle.setRetries(retries);
            sofaRuleHandle.setTimeout(timeout);
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
