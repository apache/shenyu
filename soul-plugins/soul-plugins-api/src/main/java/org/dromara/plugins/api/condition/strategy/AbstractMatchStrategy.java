/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.plugins.api.condition.strategy;

import org.dromara.soul.cache.api.data.ConditionData;
import org.dromara.plugins.api.dto.SoulRequest;

/**
 * AbstractMatchStrategy.
 *
 * @author xiaoyu(Myth)
 */
abstract class AbstractMatchStrategy {

    /**
     * Build real data string.
     *
     * @param condition the condition
     * @param soulRequest  the soulRequest
     * @return the string
     */
    String buildRealData(final ConditionData condition, SoulRequest soulRequest) {
        String realData = "";
       /* if (condition.getParamType().equals(ParamTypeEnum.QUERY.getName())) {
            final MultiValueMap<String, String> queryParams = exchange.getRequest().getQueryParams();
            realData = queryParams.getFirst(condition.getParamName());
        } else if (Objects.equals(ParamTypeEnum.HOST.getName(), condition.getParamType())) {
            realData = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getHostString();
        } else if (Objects.equals(ParamTypeEnum.IP.getName(), condition.getParamType())) {
            realData = Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
        } else if (Objects.equals(ParamTypeEnum.HEADER.getName(), condition.getParamType())) {
            final HttpHeaders headers = exchange.getRequest().getHeaders();
            final List<String> list = headers.get(condition.getParamName());
            if (CollectionUtils.isEmpty(list)) {
                return realData;
            }
            realData = Objects.requireNonNull(headers.get(condition.getParamName())).stream().findFirst().orElse("");
        } else if (condition.getParamType().equals(ParamTypeEnum.POST.getName())) {
            final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
            realData = (String) ReflectUtils.getFieldValue(requestDTO, condition.getParamName());
        } else if (condition.getParamType().equals(ParamTypeEnum.URI.getName())) {
            realData = exchange.getRequest().getURI().getPath();
        }*/
        return realData;
    }

}
