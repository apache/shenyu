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

package org.dromara.soul.web.plugin.dubbo;

import com.alibaba.dubbo.config.ApplicationConfig;
import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import com.alibaba.dubbo.config.utils.ReferenceConfigCache;
import com.alibaba.dubbo.rpc.service.GenericException;
import com.alibaba.dubbo.rpc.service.GenericService;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.DubboParamConstants;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.selector.DubboSelectorHandle;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.LogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
public class DubboProxyService {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboProxyService.class);

    private static final Map<String, RegistryConfig> REGISTRY_CONFIG_MAP = Maps.newConcurrentMap();

    private static final Map<String, ApplicationConfig> APPLICATION_CONFIG_MAP = Maps.newConcurrentMap();

    private final GenericParamService genericParamService;

    public DubboProxyService(GenericParamService genericParamService) {
        this.genericParamService = genericParamService;
    }

    /**
     * Generic invoker object.
     *
     * @param paramMap            the param map
     * @param dubboSelectorHandle the dubbo selector handle
     * @param dubboRuleHandle     the dubbo rule handle
     * @return the object
     * @throws SoulException the soul exception
     */
    public Object genericInvoker(final Map<String, Object> paramMap, final DubboSelectorHandle dubboSelectorHandle, final DubboRuleHandle dubboRuleHandle) throws SoulException {

        ReferenceConfig<GenericService> reference = buildReferenceConfig(dubboSelectorHandle, dubboRuleHandle,
                paramMap.get(DubboParamConstants.INTERFACE_NAME).toString());

        ReferenceConfigCache referenceConfigCache = ReferenceConfigCache.getCache();

        GenericService genericService;

        try {
            genericService = referenceConfigCache.get(reference);
            if (Objects.isNull(genericService)) {
                referenceConfigCache.destroy(reference);
                throw new SoulException("dubbo genericService has exception!");
            }
        } catch (NullPointerException ex) {
            referenceConfigCache.destroy(reference);
            LogUtils.error(LOGGER, ex::getMessage);
            throw new SoulException(ex.getMessage());
        }

        // 用Map表示POJO参数，如果返回值为POJO也将自动转成Map
        final String method = paramMap.get(DubboParamConstants.METHOD).toString();

        final Pair<String[], Object[]> pair = genericParamService.buildParameter(paramMap);

        try {
            // 基本类型以及Date,List,Map等不需要转换，直接调用
            return genericService.$invoke(method, pair.getLeft(), pair.getRight());
        } catch (GenericException e) {
            LogUtils.error(LOGGER, e::getExceptionMessage);
            throw new SoulException(e.getMessage());
        }
    }


    private ReferenceConfig<GenericService> buildReferenceConfig(final DubboSelectorHandle dubboSelectorHandle, final DubboRuleHandle dubboRuleHandle, final String interfaceName) {

        ReferenceConfig<GenericService> reference = new ReferenceConfig<>();

        reference.setGeneric(true);

        final ApplicationConfig applicationConfig = cacheApplication(dubboSelectorHandle.getAppName());

        reference.setApplication(applicationConfig);

        reference.setRegistry(cacheRegistry(dubboSelectorHandle.getAppName(), dubboSelectorHandle.getRegistry()));

        reference.setInterface(interfaceName);

        if (StringUtils.isNoneBlank(dubboSelectorHandle.getProtocol())) {
            reference.setProtocol(dubboSelectorHandle.getProtocol());
        }

        if (StringUtils.isNoneBlank(dubboRuleHandle.getVersion())) {
            reference.setVersion(dubboRuleHandle.getVersion());
        }

        if (StringUtils.isNoneBlank(dubboRuleHandle.getGroup())) {
            reference.setGroup(dubboRuleHandle.getGroup());
        }

        if (StringUtils.isNoneBlank(dubboRuleHandle.getLoadBalance())) {
            final String loadBalance = dubboRuleHandle.getLoadBalance();
            if (LoadBalanceEnum.HASH.getName().equals(loadBalance)) {
                reference.setLoadbalance("consistenthash");
            } else if (LoadBalanceEnum.ROUND_ROBIN.getName().equals(loadBalance)) {
                reference.setLoadbalance("roundrobin");
            } else {
                reference.setLoadbalance(loadBalance);
            }
        }

        Optional.ofNullable(dubboRuleHandle.getTimeout()).ifPresent(reference::setTimeout);

        Optional.ofNullable(dubboRuleHandle.getRetries()).ifPresent(reference::setRetries);

        return reference;
    }

    private ApplicationConfig cacheApplication(final String appName) {
        ApplicationConfig applicationConfig = APPLICATION_CONFIG_MAP.get(appName);
        if (Objects.isNull(applicationConfig)) {
            applicationConfig = new ApplicationConfig(appName);
            APPLICATION_CONFIG_MAP.put(appName, applicationConfig);
        }
        return applicationConfig;
    }

    private RegistryConfig cacheRegistry(final String appName, final String registry) {
        RegistryConfig registryConfig = REGISTRY_CONFIG_MAP.get(appName);
        if (Objects.isNull(registryConfig)) {
            registryConfig = new RegistryConfig(registry);
            REGISTRY_CONFIG_MAP.put(appName, registryConfig);
        }
        return registryConfig;
    }

}
