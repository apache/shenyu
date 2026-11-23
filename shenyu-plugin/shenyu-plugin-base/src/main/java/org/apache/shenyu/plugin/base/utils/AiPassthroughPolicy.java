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

package org.apache.shenyu.plugin.base.utils;

import org.springframework.web.server.ServerWebExchange;

/**
 * Records AI policies that consume the wire body or decorate the vendor response.
 */
public final class AiPassthroughPolicy {

    // 保存当前 exchange 上的内容处理标记所使用的属性名。
    private static final String ATTRIBUTE = AiPassthroughPolicy.class.getName();

    private AiPassthroughPolicy() {
    }

    /**
     * Mark the current exchange before consuming the body or installing a response decorator.
     *
     * @param exchange current exchange
     */
    public static void markIncompatible(final ServerWebExchange exchange) {
        exchange.getAttributes().put(ATTRIBUTE, Boolean.TRUE);
    }

    /**
     * Test whether a prior policy precludes forwarding original bytes.
     *
     * @param exchange current exchange
     * @return true when a body-processing policy was applied
     */
    public static boolean isIncompatible(final ServerWebExchange exchange) {
        return Boolean.TRUE.equals(exchange.getAttribute(ATTRIBUTE));
    }
}
