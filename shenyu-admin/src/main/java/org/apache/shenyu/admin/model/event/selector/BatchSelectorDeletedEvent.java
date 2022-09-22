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

package org.apache.shenyu.admin.model.event.selector;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.BatchChangedEvent;
import org.apache.shenyu.admin.utils.ListUtil;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * BatchPluginDeletedEvent.
 */
public class BatchSelectorDeletedEvent extends BatchChangedEvent {
    
    private final List<String> deletedIds;
    
    private final List<PluginDO> plugins;

    private final Map<String, PluginDO> pluginMap;

    private final Map<String, SelectorDO> selectorDataMap;

    /**
     * Create a new {@code BatchChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param operator operator
     * @param plugins  about plugin
     */
    public BatchSelectorDeletedEvent(final Collection<SelectorDO> source, final String operator, final List<PluginDO> plugins) {
        super(source, null, EventTypeEnum.SELECTOR_DELETE, operator);
        this.deletedIds = ListUtil.map(source, BaseDO::getId);
        this.plugins = plugins;

        this.pluginMap = ListUtil.toMap(plugins, PluginDO::getId);
        this.selectorDataMap = ListUtil.toMap(source, SelectorDO::getId);
    }
    
    @Override
    public String buildContext() {
        final String selector = ((Collection<?>) getSource())
                .stream()
                .map(s -> ((SelectorDO) s).getName())
                .collect(Collectors.joining(","));
        return String.format("the selector[%s] is %s", selector, StringUtils.lowerCase(getType().getType().toString()));
    }
    
    /**
     * get selectors.
     *
     * @return list
     */
    public List<SelectorDO> getSelectors() {
        return ((Collection<?>) getSource())
                .stream()
                .map(SelectorDO.class::cast)
                .collect(Collectors.toList());
    }

    /**
     * find plugin by selector id.
     *
     * @param selectorId  selectorId
     * @return PluginDO
     */
    public PluginDO findPluginBySelectorId(final String selectorId) {
        return Optional.ofNullable(selectorDataMap.get(selectorId))
                .flatMap(selectorDO -> Optional.ofNullable(pluginMap.get(selectorDO.getPluginId()))).orElse(null);
    }
    
    /**
     * get plugins.
     *
     * @return plugins.
     */
    public List<PluginDO> getPlugins() {
        return plugins;
    }
    
    /**
     * get deleted ids.
     *
     * @return ids.
     */
    public List<String> getDeletedIds() {
        return deletedIds;
    }
}
