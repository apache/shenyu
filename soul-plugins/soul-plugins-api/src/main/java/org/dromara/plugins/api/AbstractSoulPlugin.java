/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.plugins.api;

import lombok.RequiredArgsConstructor;
import org.dromara.plugins.api.condition.strategy.MatchStrategyFactory;
import org.dromara.soul.cache.api.data.PluginData;
import org.dromara.soul.cache.api.data.SelectorData;
import org.dromara.soul.cache.api.service.CacheService;
import org.dromara.soul.common.dto.SoulRequest;
import org.dromara.soul.common.dto.SoulResponse;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.common.utils.CollectionUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;

/**
 * abstract soul plugin please extends.
 *
 * @author xiaoyu(Myth)
 */
@RequiredArgsConstructor
public abstract class AbstractSoulPlugin implements SoulPlugin {

    /**
     * logger.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractSoulPlugin.class);

    private final CacheService cacheService;

    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param soulRequest exchange the current server exchange {@linkplain SoulRequest}
     * @param chain       chain the current chain  {@linkplain SoulPluginChain}
     * @return {@linkplain SoulResponse} to indicate when request handling is complete
     */
    protected abstract SoulResponse doExecute(SoulRequest soulRequest, SoulPluginChain chain);

    @Override
    public SoulResponse execute(SoulRequest soulRequest, SoulPluginChain chain) {
        PluginData pluginData = cacheService.findPluginByName(named());
        if (!(skip(soulRequest) || pluginData == null || !pluginData.getEnabled())) {
            //获取selector
            final List<SelectorData> selectors = cacheService.findSelectorByPluginName(named());
            if (CollectionUtils.isEmpty(selectors)) {
                LOGGER.error("can not find selector data :{},params:{}", named(), soulRequest.toString());
                return chain.execute(soulRequest);
            }
            final SelectorData selectorData = selectors.stream()
                    .filter(selector -> selector.getEnabled() && filterSelector(selector, soulRequest))
                    .findFirst().orElse(null);
            if (Objects.isNull(selectorData)) {
                LOGGER.error("can not match selector data :{},params:{}", named(), soulRequest);
                return chain.execute(soulRequest);
            }
            if (selectorData.getLoged()) {
                LogUtils.info(LOGGER, named()
                        + " selector success selector name :{}", selectorData::getName);
            }
            return doExecute(soulRequest, chain);
        }
        return chain.execute(soulRequest);
    }

    private Boolean filterSelector(final SelectorData selector, final SoulRequest soulRequest) {
        if (selector.getType() == SelectorTypeEnum.CUSTOM_FLOW.getCode()) {
            if (CollectionUtils.isEmpty(selector.getConditionList())) {
                return false;
            }
            return MatchStrategyFactory.of(selector.getMatchMode())
                    .match(selector.getConditionList(), soulRequest);
        }
        return true;
    }
}
