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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.MapUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.base.utils.PathMatchUtils;
import org.apache.shenyu.plugin.sign.api.ShenyuSignProviderWrap;
import org.apache.shenyu.plugin.sign.api.SignService;
import org.apache.shenyu.plugin.sign.api.VerifyResult;
import org.apache.shenyu.plugin.sign.api.VerifySupplier;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.server.ServerWebExchange;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Default sign service.
 */
public class DefaultSignService implements SignService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultSignService.class);

    @Value("${shenyu.sign.delay:5}")
    private int delay;

    @Override
    public VerifyResult signVerify(final ServerWebExchange exchange, final Map<String, Object> requestBody, final Map<String, String> queryParams) {
        final ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        return verify(buildSignParameters(shenyuContext), exchange, requestBody, queryParams);
    }

    private SignParameters buildSignParameters(final ShenyuContext shenyuContext) {
        return new SignParameters(
                shenyuContext.getAppKey(),
                shenyuContext.getTimestamp(),
                shenyuContext.getSign(),
                shenyuContext.getPath(),
                shenyuContext.getRealUrl(),
                shenyuContext.getContextPath());
    }

    private VerifyResult verify(final SignParameters signParameters, final ServerWebExchange exchange, final Map<String, Object> requestBody, final Map<String, String> queryParams) {

        final AppAuthData appAuthData = Optional.ofNullable(signParameters.appKey)
                .map(key -> SignAuthDataCache.getInstance().obtainAuthData(key))
                .orElse(null);

        return VerifySupplier
                .apply(() -> verifySignParameters(signParameters))
                .and(() -> verifyExpires(signParameters))
                .and(() -> verifyAuthConfig(appAuthData, signParameters))
                .and(() -> verifyPath(appAuthData, signParameters))
                .and(() -> verifySign(signParameters, appAuthData, exchange, requestBody, queryParams)).verify();
    }

    private VerifyResult verifyPath(final AppAuthData appAuthData, final SignParameters signParameters) {
        if (BooleanUtils.isNotTrue(appAuthData.getOpen())) {
            return VerifyResult.success();
        }

        List<AuthPathData> pathDataList = appAuthData.getPathDataList();
        if (CollectionUtils.isEmpty(pathDataList)) {
            LOG.error("You have not configured the sign path:{}", signParameters.appKey);
            return VerifyResult.fail(Constants.SIGN_PATH_NOT_EXIST);
        }

        boolean match = pathDataList.stream().filter(AuthPathData::getEnabled)
                .anyMatch(e -> PathMatchUtils.match(e.getPath(), signParameters.path));
        if (!match) {
            LOG.error("You have not configured the sign path:{},{}", signParameters.appKey, signParameters.realUrl);
            return VerifyResult.fail(Constants.SIGN_PATH_NOT_EXIST);
        }
        return VerifyResult.success();
    }

    private VerifyResult verifyAuthConfig(final AppAuthData appAuthData, final SignParameters signParameters) {
        if (Objects.isNull(appAuthData) || BooleanUtils.isFalse(appAuthData.getEnabled())) {
            LOG.error("sign APP_KEY does not exist or has been disabled,{}", signParameters.appKey);
            return VerifyResult.fail(Constants.SIGN_APP_KEY_IS_NOT_EXIST);
        }
        return VerifyResult.success();
    }

    private VerifyResult verifySignParameters(final SignParameters signParameters) {
        boolean success = StringUtils.isNoneBlank(signParameters.appKey)
                && StringUtils.isNoneBlank(signParameters.timestamp)
                && StringUtils.isNoneBlank(signParameters.sign);
        if (success) {
            return VerifyResult.success();
        }
        LOG.error("sign parameters are incomplete,{}", signParameters);
        return VerifyResult.fail(Constants.SIGN_PARAMS_ERROR);
    }

    private VerifyResult verifyExpires(final SignParameters signParameters) {
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestampBySystemTimezone(Long.parseLong(signParameters.timestamp));
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (Math.abs(between) <= delay) {
            return VerifyResult.success();
        }
        return VerifyResult.fail(String.format(ShenyuResultEnum.SIGN_TIME_IS_TIMEOUT.getMsg(), delay));
    }

    /**
     * verify sign .
     *
     * @param signParameters signParameters
     * @param appAuthData    appAuthData
     * @param exchange       exchange
     * @param requestBody    the request body
     * @param queryParams    url query params
     * @return verifyResult verifyResult
     */
    private VerifyResult verifySign(final SignParameters signParameters,
                                    final AppAuthData appAuthData,
                                    final ServerWebExchange exchange,
                                    final Map<String, Object> requestBody,
                                    final Map<String, String> queryParams) {

        final String sign = ShenyuSignProviderWrap.generateSign(buildExtSignKey(appAuthData.getAppSecret(), signParameters), MapUtils.transStringMap(requestBody), queryParams);
        boolean result = Objects.equals(sign, signParameters.sign);
        if (!result) {
            LOG.error("the SignUtils generated signature value is:{},the accepted value is:{}", sign, signParameters.sign);
            return VerifyResult.fail(Constants.SIGN_VALUE_IS_ERROR);
        }

        List<AuthParamData> paramDataList = appAuthData.getParamDataList();
        if (CollectionUtils.isEmpty(paramDataList)) {
            return VerifyResult.success();
        }
        paramDataList.stream().filter(p ->
                ("/" + p.getAppName()).equals(signParameters.contextPath))
                .map(AuthParamData::getAppParam)
                .filter(StringUtils::isNoneBlank).findFirst()
                .ifPresent(param -> exchange.getRequest().mutate().headers(httpHeaders -> httpHeaders.set(Constants.APP_PARAM, param)).build());

        return VerifyResult.success();
    }

    private String buildExtSignKey(final String signKey, final SignParameters signParameters) {
        return String.join("", Constants.TIMESTAMP, signParameters.timestamp, Constants.PATH, signParameters.path, Constants.VERSION, "1.0.0", signKey);
    }

    private static final class SignParameters {

        private final String appKey;

        private final String timestamp;

        private final String sign;

        private final String path;

        private final String realUrl;

        private final String contextPath;

        private SignParameters(final String appKey,
                               final String timestamp,
                               final String sign,
                               final String path,
                               final String realUrl,
                               final String contextPath) {

            this.appKey = appKey;
            this.timestamp = timestamp;
            this.sign = sign;
            this.path = path;
            this.realUrl = realUrl;
            this.contextPath = contextPath;
        }

        @Override
        public String toString() {
            return "SignParameters{" + "appKey='" + appKey
                    + '\'' + ", timestamp='" + timestamp
                    + '\'' + ", sign='" + sign + '\''
                    + '}';
        }
    }
}
