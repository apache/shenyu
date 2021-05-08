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

package org.apache.shenyu.plugin.sign.service;

import com.google.common.collect.Maps;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.PathMatchUtils;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.api.SignService;
import org.apache.shenyu.plugin.api.context.SoulContext;
import org.apache.shenyu.plugin.api.result.SoulResultEnum;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.server.ServerWebExchange;

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
            log.error("sign parameters are incomplete,{}", soulContext);
            return Pair.of(Boolean.FALSE, Constants.SIGN_PARAMS_ERROR);
        }
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestampBySystemTimezone(Long.parseLong(soulContext.getTimestamp()));
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
            log.error("sign APP_kEY does not exist or has been disabled,{}", soulContext.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_APP_KEY_IS_NOT_EXIST);
        }
        if (appAuthData.getOpen()) {
            List<AuthPathData> pathDataList = appAuthData.getPathDataList();
            if (CollectionUtils.isEmpty(pathDataList)) {
                log.error("You have not configured the sign path:{}", soulContext.getAppKey());
                return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
            }

            boolean match = pathDataList.stream().filter(AuthPathData::getEnabled)
                    .anyMatch(e -> PathMatchUtils.match(e.getPath(), soulContext.getPath()));
            if (!match) {
                log.error("You have not configured the sign path:{},{}", soulContext.getAppKey(), soulContext.getRealUrl());
                return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
            }
        }
        String sigKey = SignUtils.generateSign(appAuthData.getAppSecret(), buildParamsMap(soulContext));
        boolean result = Objects.equals(sigKey, soulContext.getSign());
        if (!result) {
            log.error("the SignUtils generated signature value is:{},the accepted value is:{}", sigKey, soulContext.getSign());
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
