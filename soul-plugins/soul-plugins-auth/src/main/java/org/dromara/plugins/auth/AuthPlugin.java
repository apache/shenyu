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

package org.dromara.plugins.auth;

import com.google.common.collect.Maps;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.dromara.plugins.api.SoulPlugin;
import org.dromara.plugins.api.SoulPluginChain;
import org.dromara.plugins.api.dto.SoulRequest;
import org.dromara.plugins.api.dto.SoulResponse;
import org.dromara.plugins.auth.utils.AuthUtils;
import org.dromara.soul.cache.api.data.AppAuthData;
import org.dromara.soul.cache.api.service.CacheService;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.utils.DateUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.Objects;

/**
 * The type Auth plugin.
 *
 * @author xiaoyu
 */
@RequiredArgsConstructor
public class AuthPlugin implements SoulPlugin {

    private static final Logger LOGGER = LoggerFactory.getLogger(AuthPlugin.class);

    private final CacheService cacheService;

    @Override
    public SoulResponse execute(SoulRequest soulRequest, SoulPluginChain chain) {
        String result = checkSignParam(soulRequest);
        if (StringUtils.isNoneBlank(result)) {

        }
        final LocalDateTime start = DateUtils.formatLocalDateTimeFromTimestamp(Long.parseLong(soulRequest.getTimestamp()));
        final LocalDateTime now = LocalDateTime.now();
        final long between = DateUtils.acquireMinutesBetween(start, now);
        if (between > 5) {
        }
        final Boolean success = signVerify(soulRequest);
        if (!success) {

        }
        return chain.execute(soulRequest);
    }

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.BEFORE;
    }

    @Override
    public int getOrder() {
        return PluginEnum.AUTH.getOrder();
    }

    @Override
    public String named() {
        return PluginEnum.AUTH.getName();
    }

    private Boolean signVerify(final SoulRequest soulRequest) {
        AppAuthData appAuthData = cacheService.findByAccessKey(soulRequest.getAccessKey());
        String sigKey = AuthUtils.getInstance().generateSign(appAuthData.getAccessKey(), buildParamsMap(soulRequest));
        boolean result = Objects.equals(sigKey, soulRequest.getSign());
        if (!result) {
            LOGGER.error("签名插件得到的签名为:{},传入的签名值为:{}", sigKey, soulRequest.getSign());
        }
        return result;
    }

    private Map<String, String> buildParamsMap(final SoulRequest dto) {
        Map<String, String> map = Maps.newHashMapWithExpectedSize(2);
        map.put(Constants.TIMESTAMP, dto.getTimestamp());
        map.put(Constants.PATH, dto.getHttpPath());
        return map;
    }


    private String checkSignParam(final SoulRequest soulRequest) {
        if (StringUtils.isBlank(soulRequest.getAccessKey())
                || StringUtils.isBlank(soulRequest.getSign()) ||
                StringUtils.isBlank(soulRequest.getTimestamp())) {
            LOGGER.error("认证参数不完整,{}", soulRequest);
            return Constants.SIGN_PARAMS_ERROR;
        }
        final AppAuthData appAuthData = cacheService.findByAccessKey(soulRequest.getAccessKey());
        if (Objects.isNull(appAuthData) || !appAuthData.getEnabled()) {
            LOGGER.error("认证APP_kEY不存在,或者已经被禁用,{}", soulRequest.getAccessKey());
            return Constants.SIGN_APP_KEY_IS_NOT_EXIST;
        }
        return "";
    }

}
