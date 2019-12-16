/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.before;

import com.alibaba.dubbo.rpc.RpcContext;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.AuthParamData;
import org.dromara.soul.common.dto.AuthPathData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.DateUtils;
import org.dromara.soul.common.utils.SignUtils;
import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.request.RequestDTO;
import org.dromara.soul.web.result.SoulResultEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class DefaultSignService implements SignService {

    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultSignService.class);

    private final LocalCacheManager localCacheManager;

    @Value("${soul.sign.delay:5}")
    private int delay;

    public DefaultSignService(final LocalCacheManager localCacheManager) {
        this.localCacheManager = localCacheManager;
    }

    @Override
    public Pair<Boolean, String> signVerify(final RequestDTO requestDTO, final ServerWebExchange exchange) {
        PluginData signData = localCacheManager.findPluginByName(PluginEnum.SIGN.getName());
        if (signData != null && signData.getEnabled()) {
            return verify(requestDTO, exchange);
        }
        return Pair.of(Boolean.TRUE, "");
    }

    private Pair<Boolean, String> verify(final RequestDTO requestDTO, final ServerWebExchange exchange) {
        if (StringUtils.isBlank(requestDTO.getAppKey())
                || StringUtils.isBlank(requestDTO.getSign())
                || StringUtils.isBlank(requestDTO.getTimestamp())) {
            LOGGER.error("认证参数不完整,{}", requestDTO);
            return Pair.of(Boolean.FALSE, Constants.SIGN_PARAMS_ERROR);
        }
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestamp(Long.parseLong(requestDTO.getTimestamp()));
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (between > delay) {
            return Pair.of(Boolean.FALSE, String.format(SoulResultEnum.SING_TIME_IS_TIMEOUT.getMsg(), delay));
        }
        return sign(requestDTO, exchange);
    }

    /**
     * verify sign .
     *
     * @param requestDTO {@linkplain RequestDTO}
     * @return result : True is pass, False is not pass.
     */
    private Pair<Boolean, String> sign(final RequestDTO requestDTO, final ServerWebExchange exchange) {
        final AppAuthData appAuthData = localCacheManager.findAuthDataByAppKey(requestDTO.getAppKey());
        if (Objects.isNull(appAuthData) || !appAuthData.getEnabled()) {
            LOGGER.error("认证APP_kEY不存在,或者已经被禁用,{}", requestDTO.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_APP_KEY_IS_NOT_EXIST);
        }
        List<AuthPathData> pathDataList = appAuthData.getPathDataList();
        if (CollectionUtils.isEmpty(pathDataList)) {
            LOGGER.error("您尚未配置路径:{}", requestDTO.getAppKey());
            return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
        }
        boolean match = pathDataList.stream().filter(AuthPathData::getEnabled)
                .anyMatch(e -> e.getPath().equals(requestDTO.getPath()));
        if (!match) {
            LOGGER.error("您尚未配置路径:{},{}", requestDTO.getAppKey(), requestDTO.getRealUrl());
            return Pair.of(Boolean.FALSE, Constants.SIGN_PATH_NOT_EXIST);
        }
        String sigKey = SignUtils.generateSign(appAuthData.getAppSecret(), buildParamsMap(requestDTO));
        boolean result = Objects.equals(sigKey, requestDTO.getSign());
        if (!result) {
            LOGGER.error("签名插件得到的签名为:{},传入的签名值为:{}", sigKey, requestDTO.getSign());
            return Pair.of(Boolean.FALSE, Constants.SIGN_VALUE_IS_ERROR);
        } else {
            List<AuthParamData> paramDataList = appAuthData.getParamDataList();
            if (CollectionUtils.isEmpty(paramDataList)) {
                return Pair.of(Boolean.TRUE, "");
            }
            paramDataList.stream().filter(p ->
                    ("/" + p.getAppName()).equals(requestDTO.getContextPath()))
                    .map(AuthParamData::getAppParam)
                    .filter(StringUtils::isNoneBlank).findFirst().ifPresent(param -> {
                        if (RpcTypeEnum.HTTP.getName().equals(requestDTO.getRpcType())) {
                            exchange.getRequest().mutate().headers(httpHeaders -> httpHeaders.set(Constants.APP_PARAM, param)).build();
                        } else if (RpcTypeEnum.DUBBO.getName().equals(requestDTO.getRpcType())) {
                            RpcContext.getContext().setAttachment(Constants.APP_PARAM, param);
                        }
                    }
            );
        }
        return Pair.of(Boolean.TRUE, "");
    }

    private Map<String, String> buildParamsMap(final RequestDTO dto) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(3);
        map.put(Constants.TIMESTAMP, dto.getTimestamp());
        map.put(Constants.PATH, dto.getPath());
        map.put(Constants.VERSION, "1.0.0");
        return map;
    }
}
