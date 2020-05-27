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

package org.dromara.soul.plugin.sign.service;

import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.AuthParamData;
import org.dromara.soul.common.dto.AuthPathData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.DateUtils;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.plugin.api.SignService;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.dromara.soul.plugin.base.cache.BaseDataCache;
import org.dromara.soul.plugin.sign.cache.SignAuthDataCache;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The type Default sign service.
 *
 * @author xiaoyu
 */
@Slf4j
public class DefaultSignService implements SignService {
    
    @Value("${soul.sign.delay:5}")
    private int delay;
    
    @Override
    public Pair<Boolean, String> signVerify(final ServerWebExchange exchange) {
        PluginData signData = BaseDataCache.getInstance().obtainPluginData(PluginEnum.SIGN.getName());
        if (signData != null && signData.getEnabled()) {
            final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
            assert soulContext != null;
            return verify(soulContext, exchange);
        }
        return Pair.of(Boolean.TRUE, "");
    }

    private Pair<Boolean, String> verify(final SoulContext soulContext, final ServerWebExchange exchange) {
        if (StringUtils.isBlank(soulContext.getAppKey())
                || StringUtils.isBlank(soulContext.getSign())
                || StringUtils.isBlank(soulContext.getTimestamp())) {
            log.error("认证参数不完整,{}", soulContext);
            return Pair.of(Boolean.FALSE, Constants.SIGN_PARAMS_ERROR);
        }
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestamp(Long.parseLong(soulContext.getTimestamp()));
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (between > delay) {
            return Pair.of(Boolean.FALSE, String.format(SoulResultEnum.SING_TIME_IS_TIMEOUT.getMsg(), delay));
        }
        return sign(soulContext, exchange);
    }

    /**
     * verify sign .
     *
     * @param soulContext {@linkplain SoulContext}
     * @return result : True is pass, False is not pass.
     */
    private Pair<Boolean, String> sign(final SoulContext soulContext, final ServerWebExchange exchange) {
        final AppAuthData appAuthData = SignAuthDataCache.getInstance().obtainAuthData(soulContext.getAppKey());
        if (Objects.isNull(appAuthData) || !appAuthData.getEnabled()) {
            log.error("认证APP_kEY不存在,或者已经被禁用,{}", soulContext.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_APP_KEY_IS_NOT_EXIST);
        }
        List<AuthPathData> pathDataList = appAuthData.getPathDataList();
        if (CollectionUtils.isEmpty(pathDataList)) {
            log.error("您尚未配置路径:{}", soulContext.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
        }
        boolean match = pathDataList.stream().filter(AuthPathData::getEnabled)
                .anyMatch(e -> e.getPath().equals(soulContext.getPath()));
        if (!match) {
            log.error("您尚未配置路径:{},{}", soulContext.getAppKey(), soulContext.getRealUrl());
            return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
        }
        String sigKey = SignUtils.generateSign(appAuthData.getAppSecret(), buildParamsMap(soulContext));
        boolean result = Objects.equals(sigKey, soulContext.getSign());
        if (!result) {
            log.error("签名插件得到的签名为:{},传入的签名值为:{}", sigKey, soulContext.getSign());
            return Pair.of(Boolean.FALSE, Constants.SIGN_VALUE_IS_ERROR);
        } else {
            List<AuthParamData> paramDataList = appAuthData.getParamDataList();
            if (CollectionUtils.isEmpty(paramDataList)) {
                return Pair.of(Boolean.TRUE, "");
            }
            paramDataList.stream().filter(p ->
                    ("/" + p.getAppName()).equals(soulContext.getContextPath()))
                    .map(AuthParamData::getAppParam)
                    .filter(StringUtils::isNoneBlank).findFirst()
                    .ifPresent(param -> exchange.getRequest().mutate().headers(httpHeaders -> httpHeaders.set(Constants.APP_PARAM, param)).build()
            );
        }
        return Pair.of(Boolean.TRUE, "");
    }

    private Map<String, String> buildParamsMap(final SoulContext dto) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(3);
        map.put(Constants.TIMESTAMP, dto.getTimestamp());
        map.put(Constants.PATH, dto.getPath());
        map.put(Constants.VERSION, "1.0.0");
        return map;
    }
}
