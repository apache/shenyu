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

package org.apache.shenyu.plugin.jwt.rule;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.SPI;
import org.springframework.web.server.ServerWebExchange;

import java.util.Map;
import java.util.Objects;

/**
 * Jwt rule handle.
 */
@SPI
public interface JwtRuleHandle extends RuleHandle {

    /**
     * init JwtRuleHandle.
     *
     * @param handleJson json string of the jwt handle.
     */
    void init(String handleJson);

    /**
     * handle exchange by config(handleJson).
     *
     * @param exchange exchange
     * @param jwtBody  jwtBody
     * @return ServerWebExchange exchange
     */
    ServerWebExchange execute(ServerWebExchange exchange, Map<String, Object> jwtBody);

    /**
     * create Jwt-Rule Handle.
     *
     * @param handleJson handleJson
     * @return JwtRuleHandle
     */
    static JwtRuleHandle newInstance(String handleJson) {
        Map<String, Object> handleMap = GsonUtils.getInstance().convertToMap(handleJson);

        Object handleType = Objects.isNull(handleMap) ? null : handleMap.get("handleType");
        JwtRuleHandle jwtRuleHandle;
        if (Objects.nonNull(handleType)) {
            jwtRuleHandle = ExtensionLoader.getExtensionLoader(JwtRuleHandle.class).getJoin(handleType.toString());
        } else {
            jwtRuleHandle = new DefaultJwtRuleHandle();
        }
        jwtRuleHandle.init(handleJson);
        return jwtRuleHandle;
    }
}
