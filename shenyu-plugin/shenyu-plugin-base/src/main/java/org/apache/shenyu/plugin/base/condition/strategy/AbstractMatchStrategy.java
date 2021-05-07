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

package org.apache.shenyu.plugin.base.condition.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.base.utils.HostAddressUtils;
import org.springframework.http.HttpCookie;
import org.springframework.util.CollectionUtils;
import org.springframework.web.server.ServerWebExchange;

import java.util.List;

/**
 * AbstractMatchStrategy.
 *
 * @author xiaoyu(Myth)
 */
public abstract class AbstractMatchStrategy {

    /**
     * Build real data string.
     *
     * @param condition the condition
     * @param exchange  the exchange
     * @return the string
     */
    public String buildRealData(final ConditionData condition, final ServerWebExchange exchange) {
        String realData = "";
        ParamTypeEnum paramTypeEnum = ParamTypeEnum.getParamTypeEnumByName(condition.getParamType());
        switch (paramTypeEnum) {
            case HEADER:
                List<String> headers = exchange.getRequest().getHeaders().get(condition.getParamName());
                if (CollectionUtils.isEmpty(headers)) {
                    return realData;
                }
                return headers.get(0);
            case URI:
                return exchange.getRequest().getURI().getPath();
            case QUERY:
                return exchange.getRequest().getQueryParams().getFirst(condition.getParamName());
            case HOST:
                return HostAddressUtils.acquireHost(exchange);
            case IP:
                return HostAddressUtils.acquireIp(exchange);
            case POST:
                SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
                return (String) ReflectUtils.getFieldValue(soulContext, condition.getParamName());
            case REQUEST_METHOD:
                return exchange.getRequest().getMethodValue();
            case COOKIE:
                List<HttpCookie> cookies = exchange.getRequest().getCookies().get(condition.getParamName());
                if (CollectionUtils.isEmpty(cookies)) {
                    return realData;
                }
                return cookies.get(0).getValue();
            default:
                break;
        }
        return realData;
    }
}
