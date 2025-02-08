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

package org.apache.shenyu.plugin.springcloud.handler;


import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.client.naming.utils.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.springcloud.cache.ServiceInstanceCache;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;

import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The type spring cloud plugin data handler.
 */
public class SpringCloudPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, SpringCloudSelectorHandle>> SELECTOR_CACHED = new BeanHolder<>(CommonHandleCache::new);

    public static final Supplier<CommonHandleCache<String, SpringCloudRuleHandle>> RULE_CACHED = new BeanHolder<>(CommonHandleCache::new);

    private static ShenyuInstanceRegisterRepository repository;

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudPluginDataHandler.class);

    private static final String NAMESPACE = "nacosNameSpace";

    private static final String GROUP = "groupName";

    private final ShenyuConfig.SpringCloudCacheConfig springCloudCacheConfig;

    private final Environment env;

    public SpringCloudPluginDataHandler(final ShenyuConfig.SpringCloudCacheConfig springCloudCacheConfig, final Environment env) {
        this.springCloudCacheConfig = springCloudCacheConfig;
        this.env = env;
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (!pluginData.getEnabled()) {
            return;
        }
        if (StringUtils.isBlank(pluginData.getConfig())) {
            // consider yml config, as eureka or nacos
            this.readYmlBuildRepository();
            return;
        }
        // get old pluginData
        PluginData oldPluginData = BaseDataCache.getInstance().obtainPluginData(pluginData.getName());
        String newConfig = pluginData.getConfig();
        String oldConfig = oldPluginData != null ? oldPluginData.getConfig() : "";
        RegisterConfig newRegisterConfig = GsonUtils.getInstance().fromJson(newConfig, RegisterConfig.class);
        if (newRegisterConfig == null) {
            return;
        }
        RegisterConfig oldRegisterConfig = null;
        if (StringUtils.isNotBlank(oldConfig)) {
            oldRegisterConfig = GsonUtils.getInstance().fromJson(oldConfig, RegisterConfig.class);
        }

        // refresh config
        if (repository == null) {
            LOG.info("springCloud handlerPlugin repository is null");
            repository = ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(newRegisterConfig);
        } else if (!newRegisterConfig.equals(oldRegisterConfig)) {
            LOG.info("springCloud handlerPlugin repository occur update");
            // the config has been updated
            if (repository != null) {
                repository.close();
            }
            repository = ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(newRegisterConfig);
        }
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getInstance().fromJson(selectorData.getHandle(), SpringCloudSelectorHandle.class);
        SELECTOR_CACHED.get().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        if (CollectionUtils.isEmpty(springCloudSelectorHandle.getDivideUpstreams())) {
            UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
            return;
        }
        if (springCloudCacheConfig.getEnabled()) {
            List<InstanceEntity> serviceInstances = repository.selectInstances(springCloudSelectorHandle.getServiceId());
            ServiceInstanceCache.cacheServiceInstance(springCloudSelectorHandle.getServiceId(), serviceInstances);
        }
        UpstreamCacheManager.getInstance().submit(selectorData.getId(), convertUpstreamList(springCloudSelectorHandle.getDivideUpstreams()));
        if (!selectorData.getContinued()) {
            RULE_CACHED.get().cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE), SpringCloudRuleHandle.newDefaultInstance());
        }
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        if (springCloudCacheConfig.getEnabled()) {
            SpringCloudSelectorHandle selectorHandle = SELECTOR_CACHED.get().obtainHandle(selectorData.getId());
            ServiceInstanceCache.removeServiceInstance(selectorHandle.getServiceId());
        }
        SELECTOR_CACHED.get().removeHandle(selectorData.getId());
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
        RULE_CACHED.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getInstance().fromJson(s, SpringCloudRuleHandle.class);
            RULE_CACHED.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), springCloudRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> RULE_CACHED.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SPRING_CLOUD.getName();
    }

    private List<Upstream> convertUpstreamList(final List<DivideUpstream> upstreamList) {
        return upstreamList.stream().map(u -> Upstream.builder()
                .protocol(u.getProtocol())
                .url(u.getUpstreamUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .warmup(u.getWarmup())
                .build()).collect(Collectors.toList());
    }

    public static ShenyuInstanceRegisterRepository getRepository() {
        return repository;
    }

    private void readYmlBuildRepository() {
        // enable cloud discovery
        if (!Boolean.parseBoolean(env.getProperty("spring.cloud.discovery.enabled"))) {
            return;
        }

        if (Boolean.parseBoolean(env.getProperty("eureka.client.enabled"))) {
            RegisterConfig registerConfig = RegisterConfig.Builder.builder()
                    .enabled(true)
                    .registerType("eureka")
                    .serverLists(env.getProperty("eureka.client.serviceUrl.defaultZone")).build();
            repository = ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(registerConfig);
            return;
        }
        if (Boolean.parseBoolean(env.getProperty("spring.cloud.nacos.discovery.enabled"))) {
            final String serverLists = env.getProperty("spring.cloud.nacos.discovery.server-addr");
            final String prefix = "spring.cloud.nacos.discovery.";
            Properties properties = new Properties();
            properties.put(NAMESPACE, env.getProperty(prefix + PropertyKeyConst.NAMESPACE));
            properties.put(GROUP, env.getProperty(prefix + "group"));
            if (env.getProperty(prefix + PropertyKeyConst.USERNAME) != null) {
                properties.put(PropertyKeyConst.USERNAME, env.getProperty(prefix + PropertyKeyConst.USERNAME));
            }
            if (env.getProperty(prefix + PropertyKeyConst.PASSWORD) != null) {
                properties.put(PropertyKeyConst.PASSWORD, env.getProperty(prefix + PropertyKeyConst.PASSWORD));
            }
            if (env.getProperty(prefix + PropertyKeyConst.ACCESS_KEY) != null) {
                properties.put(PropertyKeyConst.ACCESS_KEY, env.getProperty(prefix + PropertyKeyConst.ACCESS_KEY));
            }
            if (env.getProperty(prefix + PropertyKeyConst.SECRET_KEY) != null) {
                properties.put(PropertyKeyConst.SECRET_KEY, env.getProperty(prefix + PropertyKeyConst.SECRET_KEY));
            }
            RegisterConfig registerConfig = RegisterConfig.Builder.builder()
                    .enabled(true)
                    .registerType("nacos")
                    .serverLists(serverLists)
                    .props(properties)
                    .build();
            repository = ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(registerConfig);
        }
    }

}
