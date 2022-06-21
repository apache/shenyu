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

package org.apache.shenyu.admin.model.event.rule;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.BatchChangedEvent;
import org.apache.shenyu.admin.utils.ListUtil;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * BatchRuleDeletedEvent.
 */
public class BatchRuleDeletedEvent extends BatchChangedEvent {
    
    private final List<String> deletedIds;
    
    private final List<PluginDO> plugins;
    
    /**
     * Create a new {@code BatchChangedEvent}.operator is unknown.
     *
     * @param source   Current plugin state
     * @param operator operator
     * @param plugins  about plugin
     */
    public BatchRuleDeletedEvent(final Collection<RuleDO> source, final String operator, final List<PluginDO> plugins) {
        super(source, null, EventTypeEnum.RULE_DELETE, operator);
        this.deletedIds = ListUtil.map(source, BaseDO::getId);
        this.plugins = plugins;
    }
    
    @Override
    public String buildContext() {
        final String selector = ((Collection<?>) getSource())
                .stream()
                .map(s -> ((RuleDO) s).getName())
                .collect(Collectors.joining(","));
        return String.format("the rule[%s] is %s", selector, StringUtils.lowerCase(getType().getType().toString()));
    }
    
    /**
     * get selectors.
     *
     * @return list
     */
    public List<RuleDO> getRules() {
        return ((Collection<?>) getSource())
                .stream()
                .map(RuleDO.class::cast)
                .collect(Collectors.toList());
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
