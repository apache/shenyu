/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.condition.strategy;

import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.utils.ReflectUtils;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.support.HostAddressUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.util.CollectionUtils;
import org.springframework.util.MultiValueMap;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;
import java.util.Objects;

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
     * @param exchange  the exchange
     * @return the string
     */
    String buildRealData(final ConditionData condition, final ServerWebExchange exchange) {
        String realData = "";
        ParamTypeEnum paramTypeEnum = ParamTypeEnum.getParamTypeEnumByName(condition.getParamType());
        switch (paramTypeEnum) {
            case HEADER:
                final HttpHeaders headers = exchange.getRequest().getHeaders();
                final List<String> list = headers.get(condition.getParamName());
                if (CollectionUtils.isEmpty(list)) {
                    return realData;
                }
                realData = Objects.requireNonNull(headers.get(condition.getParamName())).stream().findFirst().orElse("");
                break;
            case URI:
                realData = exchange.getRequest().getURI().getPath();
                break;
            case QUERY:
                final MultiValueMap<String, String> queryParams = exchange.getRequest().getQueryParams();
                realData = queryParams.getFirst(condition.getParamName());
                break;
            case HOST:
                realData = HostAddressUtils.acquireHost(exchange);
                break;
            case IP:
                realData = HostAddressUtils.acquireIp(exchange);
                break;
            case POST:
                final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
                realData = (String) ReflectUtils.getFieldValue(requestDTO, condition.getParamName());
                break;
            default:
                break;
        }
        return realData;
    }

}
