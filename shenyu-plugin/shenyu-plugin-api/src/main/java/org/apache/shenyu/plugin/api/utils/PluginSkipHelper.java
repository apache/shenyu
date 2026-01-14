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

package org.apache.shenyu.plugin.api.utils;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.springframework.web.server.ServerWebExchange;

import java.util.Arrays;
import java.util.Objects;

/**
 * Plugin skip helper.
 */
public final class PluginSkipHelper {

    private PluginSkipHelper() {
    }

    /**
     * plugin is executed.
     * if return true this plugin can not execute.
     *
     * <p>the same for:
     * <pre>
     * <code>Objects.equals(rpcType, typeA.getName())
     * || Objects.equals(rpcType, typeB.getName())
     * || Objects.equals(rpcType, type...getName())
     * </code>
     * </pre>
     *
     * @param exchange the current server exchange
     * @param rpcTypes the skip rpc type list
     * @return current rpcType == someone rpcType
     */
    public static boolean skip(final ServerWebExchange exchange, final RpcTypeEnum... rpcTypes) {
        if (ArrayUtils.isEmpty(rpcTypes)) {
            return false;
        }
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        Objects.requireNonNull(shenyuContext);
        return Arrays.stream(rpcTypes).anyMatch(type -> Objects.equals(shenyuContext.getRpcType(), type.getName()));
    }

    /**
     * the plugin execute skip except some rpc types.
     * if return true this plugin can not execute.
     *
     * <p>the same for:
     * <pre>
     * <code>!Objects.equals(rpcType, typeA.getName())
     * &amp;&amp; !Objects.equals(rpcType, typeB.getName())
     * &amp;&amp; !Objects.equals(rpcType, type...getName())
     * </code>
     * </pre>
     *
     * @param exchange the current server exchange
     * @param exceptRpcTypes the except rpc type list
     * @return current rpcType != someone exceptRpcType
     */
    public static boolean skipExcept(final ServerWebExchange exchange, final RpcTypeEnum... exceptRpcTypes) {
        return !skip(exchange, exceptRpcTypes);
    }

    /**
     * Skip the non http call.
     * if return true this plugin can not execute.
     *
     * @param exchange the current server exchange
     * @return http/spring cloud/ai return false (not skipped), others return true (skipped).
     */
    public static boolean skipExceptHttpLike(final ServerWebExchange exchange) {
        return !skip(exchange, RpcTypeEnum.HTTP, RpcTypeEnum.SPRING_CLOUD, RpcTypeEnum.AI);
    }
}
