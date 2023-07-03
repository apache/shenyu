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

package org.apache.shenyu.plugin.jwt.strategy;

import org.apache.shenyu.plugin.jwt.rule.JwtRuleHandle;
import org.apache.shenyu.spi.SPI;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;

/**
 * Represents a conversion strategy that convert jwt to some attributes of
 * serverWebExchange, especially attributes of the request header.
 */
@SPI
public interface JwtConvertStrategy {

    /**
     * HandleJson needs to be parsed into jwtRuleHandle in order to
     * specify how to convert jwt.
     *
     * @param handleJson handleJson from rule
     * @return jwtRuleHandle
     */
    JwtRuleHandle parseHandleJson(String handleJson);

    /**
     * Converts jwt to some attributes of serverWebExchange based on jwtRuleHandle.
     *
     * @param jwtRuleHandle jwtRuleHandle
     * @param exchange      exchange
     * @param jwtBody       jwtBody
     * @return serverWebExchange
     */
    ServerWebExchange convert(JwtRuleHandle jwtRuleHandle, ServerWebExchange exchange, Map<String, Object> jwtBody);

}
