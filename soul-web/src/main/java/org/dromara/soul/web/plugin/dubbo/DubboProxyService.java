/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.web.plugin.dubbo;

import com.alibaba.dubbo.config.ApplicationConfig;
import com.alibaba.dubbo.config.ReferenceConfig;
import com.alibaba.dubbo.config.RegistryConfig;
import com.alibaba.dubbo.config.utils.ReferenceConfigCache;
import com.alibaba.dubbo.rpc.service.GenericException;
import com.alibaba.dubbo.rpc.service.GenericService;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.DubboParamConstants;
import org.dromara.soul.common.dto.convert.DubboHandle;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.LogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
@Service
public class DubboProxyService {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboProxyService.class);

    private static final Map<String, RegistryConfig> REGISTRY_CONFIG_MAP = Maps.newConcurrentMap();

    private static final Map<String, ApplicationConfig> APPLICATION_CONFIG_MAP = Maps.newConcurrentMap();

    /**
     * dubbo rpc invoke.
     *
     * @param paramMap    request paramMap.
     * @param dubboHandle dubboHandle.
     * @return rpc result.
     * @throws SoulException exception for rpc.
     */
    public Object genericInvoker(final Map<String, Object> paramMap, final DubboHandle dubboHandle) throws SoulException {
        ReferenceConfig<GenericService> reference = buildReferenceConfig(dubboHandle,
                paramMap.get(DubboParamConstants.INTERFACE_NAME).toString());
        ReferenceConfigCache referenceConfigCache = ReferenceConfigCache.getCache();

        GenericService genericService = referenceConfigCache.get(reference);

        // 用Map表示POJO参数，如果返回值为POJO也将自动转成Map
        final String method = paramMap.get(DubboParamConstants.METHOD).toString();

        final Pair<String[], Object[]> pair = buildParameter(paramMap);

        try {
            // 基本类型以及Date,List,Map等不需要转换，直接调用
            return genericService.$invoke(method, pair.getLeft(), pair.getRight());
        } catch (GenericException e) {
            LogUtils.error(LOGGER, e::getExceptionMessage);
            throw new SoulException(e.getMessage());
        }
    }

    private Pair<String[], Object[]> buildParameter(final Map<String, Object> paramMap) {
        List<String> paramList = Lists.newArrayList();
        List<Object> args = Lists.newArrayList();
        //如果参数里面包含class字段
        return new ImmutablePair<>(paramList.toArray(new String[0]), args.toArray());
    }

    private ReferenceConfig<GenericService> buildReferenceConfig(final DubboHandle dubboHandle, final String interfaceName) {
        ReferenceConfig<GenericService> reference = new ReferenceConfig<>();
        reference.setGeneric(true);
        final ApplicationConfig applicationConfig = cacheApplication(dubboHandle.getAppName());
        reference.setApplication(applicationConfig);
        reference.setRegistry(cacheRegistry(dubboHandle.getAppName(), dubboHandle.getRegistry()));
        reference.setInterface(interfaceName);
        if (StringUtils.isNoneBlank(dubboHandle.getVersion())) {
            reference.setVersion(dubboHandle.getVersion());
        }
        if (StringUtils.isNoneBlank(dubboHandle.getProtocol())) {
            reference.setProtocol(dubboHandle.getProtocol());
        }

        if (StringUtils.isNoneBlank(dubboHandle.getGroup())) {
            reference.setGroup(dubboHandle.getGroup());
        }

        if (StringUtils.isNoneBlank(dubboHandle.getLoadbalance())) {
            reference.setLoadbalance(dubboHandle.getLoadbalance());
        }

        Optional.ofNullable(dubboHandle.getTimeout()).ifPresent(reference::setTimeout);
        Optional.ofNullable(dubboHandle.getRetries()).ifPresent(reference::setRetries);
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
