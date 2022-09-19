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

package org.apache.shenyu.plugin.auth;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.auth.config.AuthConfig;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.casbin.casdoor.config.CasdoorConfig;
import org.casbin.casdoor.entity.CasdoorUser;
import org.casbin.casdoor.service.CasdoorAuthService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import javax.annotation.Resource;
import java.util.Objects;

public class AuthPlugin extends AbstractShenyuPlugin {

    private String[] withe = {
        "http://localhost:9195/favicon.ico",
    };

    @Resource
    private CasdoorAuthService casdoorAuthService;

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        AuthConfig authConfig = Singleton.INST.get(AuthConfig.class);
        CasdoorConfig casdoorConfig = new CasdoorConfig();
        casdoorConfig.setApplicationName(authConfig.getApplicationName());
        casdoorConfig.setCertificate(authConfig.getCertificate());
        casdoorConfig.setClientId(authConfig.getClientId());
        casdoorConfig.setEndpoint(authConfig.getEndpoint());
        casdoorConfig.setOrganizationName(authConfig.getOrganizationName());
        casdoorConfig.setClientSecret(authConfig.getClientSecret());
        CasdoorAuthService casdoorAuthService = new CasdoorAuthService(casdoorConfig);
        ServerHttpRequest request = exchange.getRequest();
        String token = exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        if (Objects.nonNull(token)) {
            CasdoorUser casdoorUser = casdoorAuthService.parseJwtToken(token);
            if (Objects.nonNull(casdoorUser)) {
                return chain.execute(handleToken(exchange, casdoorUser));
            }
        }
        org.springframework.util.MultiValueMap<String, String> queryParams = request.getQueryParams();
        String code = queryParams.getFirst("code");
        String state = queryParams.getFirst("state");
        if (Objects.nonNull(code) || Objects.nonNull(state)) {
            token = casdoorAuthService.getOAuthToken(code, state);
            CasdoorUser casdoorUser = casdoorAuthService.parseJwtToken(token);
            if (Objects.nonNull(casdoorUser)) {
                return chain.execute(handleToken(exchange, casdoorUser));
            }
        }
        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_TOKEN);
        return WebFluxResultUtils.result(exchange, error);
    }

    @Override
    public int getOrder() {
        return PluginEnum.AUTH.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AUTH.getName();
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        String uri = exchange.getRequest().getURI().toString();
        for (int i = 0; i < withe.length; i++) {
            if (uri.equals(withe[i])) {
                return true;
            }
        }
        return false;
    }

    private ServerWebExchange handleToken(final ServerWebExchange exchange, final CasdoorUser casdoorUser) {
        ServerHttpRequest.Builder mutate = exchange.getRequest().mutate();
        mutate.header("name", casdoorUser.getName());
        mutate.header("id", casdoorUser.getId());
        mutate.header("organization", casdoorUser.getOwner());
        return exchange.mutate().request(mutate.build()).build();
    }

}
