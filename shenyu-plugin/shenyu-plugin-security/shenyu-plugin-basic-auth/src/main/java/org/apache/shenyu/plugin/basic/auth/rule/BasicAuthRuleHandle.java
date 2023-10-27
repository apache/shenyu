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

package org.apache.shenyu.plugin.basic.auth.rule;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.basic.auth.strategy.BasicAuthAuthenticationStrategy;
import org.apache.shenyu.plugin.basic.auth.strategy.BasicAuthAuthenticationStrategyFactory;

import java.util.Map;
import java.util.Objects;

/**
 * BasicAuth rule handle.
 */

public class BasicAuthRuleHandle implements RuleHandle {

    private String handleType;

    private BasicAuthAuthenticationStrategy basicAuthAuthenticationStrategy;

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
     * get basicAuthAuthenticationStrategy.
     *
     * @return basicAuthAuthenticationStrategy
     */
    public BasicAuthAuthenticationStrategy getBasicAuthAuthenticationStrategy() {
        return basicAuthAuthenticationStrategy;
    }

    /**
     * set basicAuthAuthenticationStrategy.
     *
     * @param basicAuthAuthenticationStrategy basicAuthAuthenticationStrategy
     */
    public void setBasicAuthAuthenticationStrategy(final BasicAuthAuthenticationStrategy basicAuthAuthenticationStrategy) {
        this.basicAuthAuthenticationStrategy = basicAuthAuthenticationStrategy;
    }


    /**
     * new instance basicAuthRuleHandle.
     *
     * @param handleJson handleJson from rule
     * @return basicAuthRuleHandle
     */
    public static BasicAuthRuleHandle newInstance(final String handleJson) {
        if (Objects.isNull(handleJson)) {
            return null;
        }
        Map<String, Object> handleMap = GsonUtils.getInstance().convertToMap(handleJson);
        String handleType = null;
        if (Objects.nonNull(handleMap)) {
            handleType = handleMap.getOrDefault("handleType", "default").toString();
        }
        BasicAuthAuthenticationStrategy basicAuthAuthenticationStrategy1 = BasicAuthAuthenticationStrategyFactory.newInstance(handleType);
        BasicAuthRuleHandle basicAuthRuleHandle = basicAuthAuthenticationStrategy1.parseHandleJson(handleJson);
        basicAuthRuleHandle.setBasicAuthAuthenticationStrategy(basicAuthAuthenticationStrategy1);
        return basicAuthRuleHandle;
    }

}
