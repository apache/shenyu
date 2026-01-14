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

package org.apache.shenyu.plugin.api;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.utils.PluginSkipHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * the shenyu plugin interface.
 */
public interface ShenyuPlugin {

    Logger LOG = LoggerFactory.getLogger(ShenyuPlugin.class);

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link ShenyuPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    Mono<Void> execute(ServerWebExchange exchange, ShenyuPluginChain chain);

    /**
     * return plugin order .
     * This attribute To determine the plugin execution order in the same type plugin.
     *
     * @return int order
     */
    int getOrder();

    /**
     * acquire plugin name.
     * this is plugin name define you must Provide the right name.
     * if you impl AbstractShenyuPlugin this attribute not use.
     *
     * @return plugin name.
     */
    default String named() {
        return "";
    }

    /**
     * plugin is executed.
     * if return true this plugin can not execute.
     *
     * @param exchange the current server exchange
     * @return default false.
     */
    default boolean skip(ServerWebExchange exchange) {
        return false;
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
    default boolean skip(ServerWebExchange exchange, RpcTypeEnum... rpcTypes) {
        return PluginSkipHelper.skip(exchange, rpcTypes);
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
    default boolean skipExcept(ServerWebExchange exchange, RpcTypeEnum... exceptRpcTypes) {
        return PluginSkipHelper.skipExcept(exchange, exceptRpcTypes);
    }

    /**
     * Skip the non http call.
     * if return true this plugin can not execute.
     *
     * @param exchange the current server exchange
     * @return http/spring cloud/ai return false (not skipped), others return true (skipped).
     */
    default boolean skipExceptHttpLike(ServerWebExchange exchange) {
        return PluginSkipHelper.skipExceptHttpLike(exchange);
    }

    /**
     * Plugin before operation.
     *
     * @param exchange context
     */
    default void before(ServerWebExchange exchange) {
        exchange.getAttributes().put(Constants.PLUGIN_START_TIME + named(), System.currentTimeMillis());
    }

    /**
     * Plugin after operation.
     *
     * @param exchange context
     */
    default void after(ServerWebExchange exchange) {
        Boolean loggingEnabled = exchange.getAttributeOrDefault(Constants.LOGGING_ENABLED, true);
        if (!loggingEnabled) {
            return;
        }
        long currentTimeMillis = System.currentTimeMillis();
        Long startTime = exchange.getAttribute(Constants.PLUGIN_START_TIME + named());
        if (startTime == null) {
            exchange.getAttributes().remove(Constants.PLUGIN_START_TIME + named());
            return;
        }
        long cost = currentTimeMillis - startTime;
        Long minCost = exchange.getAttributeOrDefault(Constants.LOGGING_MIN_COST, 0L);
        if (cost >= minCost) {
            LOG.debug("shenyu traceId:{}, plugin named:{}, cost:{}", exchange.getLogPrefix(), named(), cost);
        }
        exchange.getAttributes().remove(Constants.PLUGIN_START_TIME + named());
    }
}

