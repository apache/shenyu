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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.PathMatchUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.sign.api.ShenyuSignProviderWrap;
import org.apache.shenyu.plugin.sign.api.SignService;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.ObjectUtils;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The type Default sign service.
 */
public class DefaultSignService implements SignService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultSignService.class);

    @Value("${shenyu.sign.delay:5}")
    private int delay;

    @Override
    public Pair<Boolean, String> signVerify(final ServerWebExchange exchange, final Map<String, Object> requestBody) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return verify(shenyuContext, exchange, requestBody);
    }

    private Pair<Boolean, String> verify(final ShenyuContext shenyuContext, final ServerWebExchange exchange, final Map<String, Object> requestBody) {
        if (StringUtils.isBlank(shenyuContext.getAppKey())
                || StringUtils.isBlank(shenyuContext.getSign())
                || StringUtils.isBlank(shenyuContext.getTimestamp())) {
            LOG.error("sign parameters are incomplete,{}", shenyuContext);
            return Pair.of(Boolean.FALSE, Constants.SIGN_PARAMS_ERROR);
        }
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestampBySystemTimezone(Long.parseLong(shenyuContext.getTimestamp()));
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (between > delay) {
            return Pair.of(Boolean.FALSE, String.format(ShenyuResultEnum.SIGN_TIME_IS_TIMEOUT.getMsg(), delay));
        }

        return sign(shenyuContext, exchange, requestBody);
    }

    /**
     * verify sign .
     *
     * @param shenyuContext {@linkplain ShenyuContext}
     * @return result : True is pass, False is not pass.
     */
    private Pair<Boolean, String> sign(final ShenyuContext shenyuContext, final ServerWebExchange exchange, final Map<String, Object> requestBody) {
        final AppAuthData appAuthData = SignAuthDataCache.getInstance().obtainAuthData(shenyuContext.getAppKey());
        if (Objects.isNull(appAuthData) || Boolean.FALSE.equals(appAuthData.getEnabled())) {
            LOG.error("sign APP_kEY does not exist or has been disabled,{}", shenyuContext.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_APP_KEY_IS_NOT_EXIST);
        }
        if (Boolean.TRUE.equals(appAuthData.getOpen())) {
            List<AuthPathData> pathDataList = appAuthData.getPathDataList();
            if (CollectionUtils.isEmpty(pathDataList)) {
                LOG.error("You have not configured the sign path:{}", shenyuContext.getAppKey());
                return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
            }

            boolean match = pathDataList.stream().filter(AuthPathData::getEnabled)
                    .anyMatch(e -> PathMatchUtils.match(e.getPath(), shenyuContext.getPath()));
            if (!match) {
                LOG.error("You have not configured the sign path:{},{}", shenyuContext.getAppKey(), shenyuContext.getRealUrl());
                return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
            }
        }
        String sigKey = ShenyuSignProviderWrap.generateSign(appAuthData.getAppSecret(), buildParamsMap(shenyuContext, requestBody));
        boolean result = Objects.equals(sigKey, shenyuContext.getSign());
        if (!result) {
            LOG.error("the SignUtils generated signature value is:{},the accepted value is:{}", sigKey, shenyuContext.getSign());
            return Pair.of(Boolean.FALSE, Constants.SIGN_VALUE_IS_ERROR);
        } else {
            List<AuthParamData> paramDataList = appAuthData.getParamDataList();
            if (CollectionUtils.isEmpty(paramDataList)) {
                return Pair.of(Boolean.TRUE, "");
            }
            paramDataList.stream().filter(p ->
                    ("/" + p.getAppName()).equals(shenyuContext.getContextPath()))
                    .map(AuthParamData::getAppParam)
                    .filter(StringUtils::isNoneBlank).findFirst()
                    .ifPresent(param -> exchange.getRequest().mutate().headers(httpHeaders -> httpHeaders.set(Constants.APP_PARAM, param)).build()
            );
        }
        return Pair.of(Boolean.TRUE, "");
    }

    private Map<String, String> buildParamsMap(final ShenyuContext shenyuContext, final Map<String, Object> requestBody) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(3);
        map.put(Constants.TIMESTAMP, shenyuContext.getTimestamp());
        map.put(Constants.PATH, shenyuContext.getPath());
        map.put(Constants.VERSION, "1.0.0");
        if (!ObjectUtils.isEmpty(requestBody)) {
            requestBody.forEach((key, value) -> {
                map.putIfAbsent(key, Objects.toString(value, null));
            });
        }
        return map;
    }
}
