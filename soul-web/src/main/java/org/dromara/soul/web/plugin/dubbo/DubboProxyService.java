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
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.DubboParamConstants;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.selector.DubboSelectorHandle;
import org.dromara.soul.common.enums.LoadBalanceEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.GSONUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * dubbo proxy service is  use GenericService.
 *
 * @author xiaoyu(Myth)
 */
@Service
@SuppressWarnings("all")
public class DubboProxyService {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(DubboProxyService.class);

    private static final Map<String, RegistryConfig> REGISTRY_CONFIG_MAP = Maps.newConcurrentMap();

    private static final Map<String, ApplicationConfig> APPLICATION_CONFIG_MAP = Maps.newConcurrentMap();

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

        GenericService genericService = null;

        try {
            genericService = referenceConfigCache.get(reference);
        } catch (NullPointerException ex) {
            referenceConfigCache.destroy(reference);
            LogUtils.error(LOGGER, ex::getMessage);
            throw new SoulException(ex.getMessage());
        }

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
        //如果参数里面包含class字段
        if (paramMap.containsKey(DubboParamConstants.PARAM_CLASS)) {
            List<String> clazz = GSONUtils.getInstance()
                    .fromJson(paramMap.get(DubboParamConstants.PARAM_CLASS).toString(), List.class);
            //设置参数为class 类型
            AtomicBoolean hasList = new AtomicBoolean(false);
            clazz.forEach(c -> {
                paramList.add(c.toString());
                if (List.class.getName().equals(c.toString())) {
                    hasList.set(true);
                }
            });

            if (hasList.get()) {
                final Object classParams = paramMap.get(DubboParamConstants.CLASS_PARAMS);
                List<Map> params = GSONUtils.getInstance().toListMap(classParams.toString());
                args.add(params);
            } else {
                final Object classParams = paramMap.get(DubboParamConstants.CLASS_PARAMS);
                args.addAll(GSONUtils.getInstance()
                        .fromJson(classParams.toString(), List.class));
            }
        }
        //如果Map参数里面包含 params字段  规定params 里面是json字符串转成Map key为类型，value为值
        if (paramMap.containsKey(DubboParamConstants.PARAMS)) {
            final Object params = paramMap.get(DubboParamConstants.PARAMS);
            final Map<String, Object> objectMap = GSONUtils.getInstance().toObjectMap(params.toString());
            objectMap.forEach((k, v) -> {
                //如果v是数组类型
                if (v instanceof List) {
                    List<String> arg = GSONUtils.getInstance().fromJson(v.toString(), List.class);
                    arg.forEach(j -> {
                        paramList.add(k);
                        args.add(j);
                    });
                } else {
                    paramList.add(k);
                    args.add(v);
                }
            });
        }
        return new ImmutablePair<>(paramList.toArray(new String[0]), args.toArray());
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
