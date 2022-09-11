// Copyright 2022 The casdoor Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package org.apache.shenyu.plugin.auth;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.casbin.casdoor.entity.CasdoorUser;
import org.casbin.casdoor.service.CasdoorAuthService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import javax.annotation.Resource;

public class AuthPlugin implements ShenyuPlugin {

    @Resource
    private CasdoorAuthService casdoorAuthService;

    String withe[] = {"http://localhost:9195/http/hi","http://localhost:9195/favicon.ico"};

    @Override
    public reactor.core.publisher.Mono<Void> execute(org.springframework.web.server.ServerWebExchange exchange, ShenyuPluginChain chain) {
        ServerHttpRequest request = exchange.getRequest();
        String token =  exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        if(token!=null){
            CasdoorUser casdoorUser = casdoorAuthService.parseJwtToken(token);
            exchange = handleToken(exchange,casdoorUser);
            if(casdoorUser!=null){
                return chain.execute(exchange);
            }
        }
        org.springframework.util.MultiValueMap<String, String> queryParams = request.getQueryParams();
        String code = queryParams.getFirst("code");
        String state = queryParams.getFirst("state");
        if(code!=null||state!=null){
            token = casdoorAuthService.getOAuthToken(code, state);
            CasdoorUser casdoorUser = casdoorAuthService.parseJwtToken(token);
            if (token!=null){
                exchange = handleToken(exchange,casdoorUser);
                return chain.execute(exchange);
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
    public boolean skip(org.springframework.web.server.ServerWebExchange exchange) {
        String uri = exchange.getRequest().getURI().toString();
        for (int i=0;i<withe.length;i++){
            if(uri.equals(withe[i])){
                return true;
            }
        }
        return false;
    }
    private ServerWebExchange handleToken(final ServerWebExchange exchange, final CasdoorUser casdoorUser) {
        ServerHttpRequest.Builder mutate = exchange.getRequest().mutate();
        mutate.header("name",casdoorUser.getName());
        mutate.header("id",casdoorUser.getId());
        mutate.header("organization",casdoorUser.getOwner());
        return exchange.mutate().request(mutate.build()).build();
    }
}