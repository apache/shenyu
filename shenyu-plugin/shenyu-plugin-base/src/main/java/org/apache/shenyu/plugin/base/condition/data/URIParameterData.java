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

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The type URI parameter data.
 */
@Join
public class URIParameterData implements ParameterData {

    @Override
    public String builder(final String paramName, final ServerWebExchange exchange) {
        return exchange.getRequest().getURI().getPath();
    }

    @Override
    public Boolean containsJudge(ConditionData conditionData, String realData) {
        String regex = ",";
        String[] conditionUriRules = conditionData.getParamValue().trim().split(regex);
        List<String> fitRules = Arrays.stream(conditionUriRules).filter(conditionUriRule -> realData.contains(conditionUriRule.trim())).collect(Collectors.toList());
        return CollectionUtils.isNotEmpty(fitRules);
    }
}
