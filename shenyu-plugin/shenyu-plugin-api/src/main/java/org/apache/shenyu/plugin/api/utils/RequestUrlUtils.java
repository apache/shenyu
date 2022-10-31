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

package org.apache.shenyu.plugin.api.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;

/**
 * RequestUrlUtils.
 */
public final class RequestUrlUtils {

    private RequestUrlUtils() {
    }

    /**
     * Build the final request uri.
     *
     * @param exchange the exchange
     * @param domain   the domain
     * @return request uri
     */
    public static URI buildRequestUri(final ServerWebExchange exchange, final String domain) {
        String path = domain;
        final String rewriteUri = (String) exchange.getAttributes().get(Constants.REWRITE_URI);
        if (StringUtils.isNoneBlank(rewriteUri)) {
            path = path + rewriteUri;
        } else {
            ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
            assert shenyuContext != null;
            String realUrl = shenyuContext.getRealUrl();
            if (StringUtils.isNoneBlank(realUrl)) {
                path = path + realUrl;
            }
        }
        if (StringUtils.isNoneBlank(exchange.getRequest().getURI().getQuery())) {
            path = String.join("?", path, RequestQueryCodecUtil.getCodecQuery(exchange));
        }
        return URI.create(path);
    }
}
