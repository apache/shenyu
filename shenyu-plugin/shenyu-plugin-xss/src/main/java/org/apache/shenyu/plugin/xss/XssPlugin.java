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

package org.apache.shenyu.plugin.xss;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * XSS plugin avoids common XSS attacks.
 */
public class XssPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(XssPlugin.class);


    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        LOG.info("Xss plugin doExecute start.");

        // Http only 过滤
        ServerHttpResponse response = exchange.getResponse();
        HttpHeaders headers = response.getHeaders();
        List<String> cookies = headers.get(HttpHeaders.SET_COOKIE);
        if (cookies != null) {
            List<String> newCookies = new ArrayList<>(cookies.size());
            for (String cookie : cookies) {
                if (cookie.contains("JSESSIONID")) {
                    newCookies.add(cookie + "; HttpOnly");
                } else {
                    newCookies.add(cookie);
                }
            }
            headers.put(HttpHeaders.SET_COOKIE, newCookies);
        }
        // Http only 过滤


        LOG.info("Xss plugin doExecute end.");
        return chain.execute(exchange);
    }


    @Override
    public int getOrder() {
        return PluginEnum.XSS.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.XSS.getName();
    }
}
