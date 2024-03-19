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

package org.apache.shenyu.plugin.ratelimiter.resolver;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

@Join
public class RemoteAddrKeyResolver implements RateLimiterKeyResolver {

    @Override
    public String getKeyResolverName() {
        return "REMOTE_ADDRESS_KEY_RESOLVER";
    }

    @Override
    public String resolve(final ServerWebExchange exchange) {
        String[] headers = {"X-Forwarded-For", "X-Real-IP", "Proxy-Client-IP", "WL-Proxy-Client-IP", "HTTP_CLIENT_IP", "HTTP_X_FORWARDED_FOR"};
        String ip;
        for (String header : headers) {
            ip = exchange.getRequest().getHeaders().getFirst(header);
            boolean isUnknown = StringUtils.isBlank(ip) || "unknown".equalsIgnoreCase(ip);
            if (!isUnknown) {
                if (StringUtils.indexOf(ip, ',') > 0) {
                    String[] split = StringUtils.split(ip, ',');
                    for (int i = 0; i < split.length; i++) {
                        split[i] = split[i].trim();
                    }
                    for (final String subIp : split) {
                        boolean isUnknownSubIp = StringUtils.isBlank(subIp) || "unknown".equalsIgnoreCase(subIp);
                        if (!isUnknownSubIp) {
                            ip = subIp;
                            break;
                        }
                    }
                }
                return ip;
            }
        }
        return Objects.requireNonNull(exchange.getRequest().getRemoteAddress()).getAddress().getHostAddress();
    }

}
