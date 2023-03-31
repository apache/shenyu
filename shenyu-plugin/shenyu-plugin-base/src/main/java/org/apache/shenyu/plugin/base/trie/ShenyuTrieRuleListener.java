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

package org.apache.shenyu.plugin.base.trie;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.RuleTrieEventEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.event.RuleTrieEvent;
import org.springframework.context.ApplicationListener;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * shenyu trie rule change listener.
 */
public class ShenyuTrieRuleListener implements ApplicationListener<RuleTrieEvent> {

    @Override
    public void onApplicationEvent(final RuleTrieEvent event) {
        RuleTrieEventEnum eventEnum = event.getRuleTrieEvent();
        RuleData ruleData = (RuleData) event.getSource();
        // new condition
        List<ConditionData> conditionDataList = ruleData.getConditionDataList();
        List<ConditionData> filterConditions = Optional.ofNullable(conditionDataList).orElse(Collections.emptyList())
                .stream().filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                .collect(Collectors.toList());

        if (CollectionUtils.isNotEmpty(filterConditions)) {
            List<String> uriPaths = filterConditions.stream().map(ConditionData::getParamValue).collect(Collectors.toList());
            final ShenyuTrie shenyuTrie = SpringBeanUtils.getInstance().getBean(ShenyuTrie.class);
            switch (eventEnum) {
                case INSERT:
                    synchronized (ruleData.getId()) {
                        // insert rule data must remove original rule, and the operation must atomic
                        shenyuTrie.remove(uriPaths, ruleData);
                        shenyuTrie.putNode(uriPaths, ruleData, ruleData.getId());
                    }
                    break;
                case UPDATE:
                    final List<ConditionData> beforeConditionDataList = ruleData.getBeforeConditionDataList();
                    List<String> beforeUriPaths = beforeConditionDataList.stream()
                            .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                            .map(ConditionData::getParamValue)
                            .collect(Collectors.toList());

                    // old condition remove
                    shenyuTrie.remove(beforeUriPaths, ruleData);
                    shenyuTrie.putNode(uriPaths, ruleData, ruleData.getId());
                    break;
                case REMOVE:
                    shenyuTrie.remove(uriPaths, ruleData);
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + event.getRuleTrieEvent());
            }
        }
    }
}
