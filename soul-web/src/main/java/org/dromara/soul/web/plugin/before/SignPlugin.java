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
import org.dromara.soul.common.dto.zk.AppAuthZkDTO;
import org.dromara.soul.common.dto.zk.PluginZkDTO;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.JsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.dromara.soul.web.plugin.SoulPlugin;
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
public class SignPlugin implements SoulPlugin {

    private static final Logger LOGGER = LoggerFactory.getLogger(SignPlugin.class);

    private final ZookeeperCacheManager zookeeperCacheManager;

    /**
     * Instantiates a new Sign plugin.
     *
     * @param zookeeperCacheManager the zookeeper cache manager
     */
    public SignPlugin(final ZookeeperCacheManager zookeeperCacheManager) {
        this.zookeeperCacheManager = zookeeperCacheManager;
    }

    @Override
    public String named() {
        return PluginEnum.SIGN.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.SIGN.getCode();
    }

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code WebFilter} through the given {@link SoulPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next filter
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final SoulPluginChain chain) {
        final PluginZkDTO pluginZkDTO =
                zookeeperCacheManager.findPluginByName(named());
        if (pluginZkDTO != null && pluginZkDTO.getEnabled()) {
            final RequestDTO requestDTO = exchange.getAttribute(Constants.REQUESTDTO);
            assert requestDTO != null;
            final Boolean success = signVerify(requestDTO);
            if (!success) {
                exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                final SoulResult error = SoulResult.error(HttpStatus.UNAUTHORIZED.value(), Constants.SIGN_IS_NOT_PASS);
                return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                        .bufferFactory().wrap(Objects.requireNonNull(JsonUtils.toJson(error)).getBytes())));
            }
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
        final AppAuthZkDTO appAuthZkDTO = zookeeperCacheManager.findAuthDTOByAppKey(requestDTO.getAppKey());
        if (Objects.isNull(appAuthZkDTO)
                || StringUtils.isBlank(requestDTO.getSign())
                || StringUtils.isBlank(requestDTO.getAppKey())
                || StringUtils.isBlank(appAuthZkDTO.getAppKey())
                || StringUtils.isBlank(appAuthZkDTO.getAppSecret())
                || !appAuthZkDTO.getEnabled()) {
            LogUtils.error(LOGGER, () -> requestDTO.getAppKey() + " can not config!");
            return false;
        }
        return SignUtils.getInstance().isValid(requestDTO.getSign(), buildParamsMap(requestDTO), appAuthZkDTO.getAppSecret());
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
        map.put("timestamp", dto.getTimestamp());
        map.put("module", dto.getModule());
        map.put("method", dto.getMethod());
        map.put("rpcType", dto.getRpcType());
        return map;
    }

}
