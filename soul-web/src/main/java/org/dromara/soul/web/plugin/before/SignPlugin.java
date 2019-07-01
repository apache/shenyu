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

package org.dromara.soul.web.plugin.before;

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.plugin.AbstractSoulPlugin;
import org.dromara.soul.web.plugin.SoulPluginChain;
import org.dromara.soul.web.request.RequestDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.Objects;

/**
 * Sign Plugin.
 *
 * @author xiaoyu(Myth)
 */
public class SignPlugin extends AbstractSoulPlugin {

    private static final Logger LOGGER = LoggerFactory.getLogger(SignPlugin.class);

    private LocalCacheManager localCacheManager;


    /**
     * Instantiates a new Sign plugin.
     *
     * @param localCacheManager the local cache manager
     */
    public SignPlugin(final LocalCacheManager localCacheManager) {
        super(localCacheManager);
        this.localCacheManager = localCacheManager;
    }

    @Override
    public String named() {
        return PluginEnum.SIGN.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SIGN.getCode();
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, final RuleData rule) {
        final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
        final Boolean success = signVerify(Objects.requireNonNull(requestDTO));
        if (!success) {
            exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
            final SoulResult error = SoulResult.error(HttpStatus.UNAUTHORIZED.value(), Constants.SIGN_IS_NOT_PASS);
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                    .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.toJson(error)).getBytes())));
        }
        return chain.execute(exchange);
    }

    /**
     * verify sign .
     *
     * @param requestDTO {@linkplain RequestDTO}
     * @return result : True is pass, False is not pass.
     */
    private Boolean signVerify(final RequestDTO requestDTO) {
        if (StringUtils.isBlank(requestDTO.getAppKey())) {
            LogUtils.error(LOGGER, () -> " app key can not incoming!");
            return false;
        }
        final AppAuthData appAuthData = localCacheManager.findAuthDataByAppKey(requestDTO.getAppKey());
        if (Objects.isNull(appAuthData)
                || StringUtils.isBlank(requestDTO.getSign())
                || StringUtils.isBlank(requestDTO.getAppKey())
                || StringUtils.isBlank(appAuthData.getAppKey())
                || StringUtils.isBlank(appAuthData.getAppSecret())
                || !appAuthData.getEnabled()) {
            LogUtils.error(LOGGER, () -> requestDTO.getAppKey() + " can not configuration!");
            return false;
        }
        return SignUtils.getInstance().isValid(requestDTO.getSign(),
                buildParamsMap(requestDTO), appAuthData.getAppSecret());
    }

    /**
     * return plugin type.
     *
     * @return {@linkplain PluginTypeEnum}
     */
    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.BEFORE;
    }

    private Map<String, String> buildParamsMap(final RequestDTO dto) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(4);
        map.put(Constants.TIMESTAMP, dto.getTimestamp());
        map.put(Constants.MODULE, dto.getModule());
        map.put(Constants.METHOD, dto.getMethod());
        map.put(Constants.RPC_TYPE, dto.getRpcType());
        return map;
    }

}
