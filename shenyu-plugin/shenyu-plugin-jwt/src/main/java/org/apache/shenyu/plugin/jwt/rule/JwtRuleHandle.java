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
import org.apache.shenyu.plugin.jwt.strategy.JwtConvertStrategy;
import org.apache.shenyu.plugin.jwt.strategy.JwtConvertStrategyFactory;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

/**
 * Jwt rule handle.
 */

public class JwtRuleHandle implements RuleHandle, Serializable {

    private static final long serialVersionUID = -3023493891692468701L;

    private static final String DEFAULT_JWT_CONVERTER = "default";

    private String handleType;

    /**
     * get handleType.
     *
     * @return handleType
     */
    public String getHandleType() {
        return handleType;
    }

    /**
     * set handleType.
     *
     * @param handleType handleType
     */
    public void setHandleType(final String handleType) {
        this.handleType = handleType;
    }

    /**
     * new instance jwtRuleHandle.
     *
     * @param handleJson handleJson from rule
     * @return jwtRuleHandle
     */
    public static JwtRuleHandle newInstance(final String handleJson) {
        if (Objects.isNull(handleJson)) {
            return null;
        }
        Map<String, Object> handleMap = GsonUtils.getInstance().convertToMap(handleJson);
        String handleType = null;
        if (Objects.nonNull(handleMap)) {
            handleType = handleMap.getOrDefault("handleType", DEFAULT_JWT_CONVERTER).toString();
        }
        JwtConvertStrategy convertStrategy = JwtConvertStrategyFactory.newInstance(handleType);
        return convertStrategy.parseHandleJson(handleJson);
    }

}
