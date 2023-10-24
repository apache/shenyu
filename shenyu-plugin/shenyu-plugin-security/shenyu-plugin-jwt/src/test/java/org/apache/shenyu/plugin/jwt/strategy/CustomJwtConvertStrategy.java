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

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.jwt.rule.CustomJwtRuleHandle;
import org.apache.shenyu.plugin.jwt.rule.JwtRuleHandle;
import org.apache.shenyu.spi.Join;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;

@Join
public class CustomJwtConvertStrategy implements JwtConvertStrategy {

    @Override
    public CustomJwtRuleHandle parseHandleJson(final String handleJson) {

        return GsonUtils.getInstance().fromJson(handleJson, CustomJwtRuleHandle.class);
    }

    @Override
    public ServerWebExchange convert(final JwtRuleHandle jwtRuleHandle, final ServerWebExchange exchange, final Map<String, Object> jwtBody) {
        final CustomJwtRuleHandle customJwtRuleHandle = (CustomJwtRuleHandle) jwtRuleHandle;
        String customConvert = customJwtRuleHandle.getCustomConvert();
        ServerHttpRequest modifiedRequest =
                exchange.getRequest().mutate().header("custom", customConvert).build();

        return exchange.mutate().request(modifiedRequest).build();
    }
}
