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
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieEventEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.event.TrieEvent;
import org.springframework.context.ApplicationListener;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * shenyu trie rule change listener.
 */
public class ShenyuTrieListener implements ApplicationListener<TrieEvent> {
    
    private static final Object LOCK = new Object();

    @Override
    public void onApplicationEvent(final TrieEvent event) {
        TrieEventEnum eventEnum = event.getTrieEventEnum();
        TrieCacheTypeEnum cacheTypeEnum = event.getTrieCacheTypeEnum();
        Object source = event.getSource();
        
        ShenyuTrie shenyuTrie;
        RuleData ruleData;
        SelectorData selectorData;
        List<ConditionData> conditionDataList;
        if (TrieCacheTypeEnum.RULE.equals(cacheTypeEnum)) {
            ruleData = (RuleData) source;
            conditionDataList = ruleData.getConditionDataList();
            shenyuTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        } else if (TrieCacheTypeEnum.SELECTOR.equals(cacheTypeEnum)) {
            shenyuTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
            selectorData = (SelectorData) source;
            conditionDataList = selectorData.getConditionList();
        } else {
            throw new IllegalStateException("Unexpected value: " + event.getTrieEventEnum());
        }
        
        List<ConditionData> filterConditions = Optional.ofNullable(conditionDataList).orElse(Collections.emptyList())
                .stream().filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                .collect(Collectors.toList());

        if (CollectionUtils.isNotEmpty(filterConditions)) {
            List<String> uriPaths = filterConditions.stream().map(ConditionData::getParamValue).collect(Collectors.toList());
            switch (eventEnum) {
                case INSERT:
                    insertTrieNode(uriPaths, source, cacheTypeEnum, shenyuTrie);
                    break;
                case UPDATE:
                    updateTrieNode(uriPaths, source, cacheTypeEnum, shenyuTrie);
                    break;
                case REMOVE:
                    removeTrieNode(uriPaths, source, cacheTypeEnum, shenyuTrie);
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + event.getTrieEventEnum());
            }
        }
    }
    
    private <T> void insertTrieNode(final List<String> uriPaths, final T data, final TrieCacheTypeEnum cacheTypeEnum, final ShenyuTrie trie) {
        synchronized (LOCK) {
            trie.remove(uriPaths, data, cacheTypeEnum);
            trie.putNode(uriPaths, data, cacheTypeEnum);
        }
    }
    
    private <T> void updateTrieNode(final List<String> uriPaths, final T data, final TrieCacheTypeEnum cacheTypeEnum, final ShenyuTrie trie) {
        final List<ConditionData> beforeConditionDataList;
        if (TrieCacheTypeEnum.RULE.equals(cacheTypeEnum)) {
            RuleData ruleData = (RuleData) data;
            beforeConditionDataList = ruleData.getBeforeConditionDataList();
        } else {
            SelectorData selectorData = (SelectorData) data;
            beforeConditionDataList = selectorData.getBeforeConditionList();
        }
        // filter before uri condition
        List<String> beforeUriPaths = beforeConditionDataList.stream()
                .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                .map(ConditionData::getParamValue)
                .collect(Collectors.toList());
        
        // old condition remove
        synchronized (LOCK) {
            trie.remove(beforeUriPaths, data, cacheTypeEnum);
            trie.putNode(uriPaths, data, cacheTypeEnum);
        }
    }
    
    private <T> void removeTrieNode(final List<String> uriPaths, final T data, final TrieCacheTypeEnum cacheTypeEnum, final ShenyuTrie trie) {
        trie.remove(uriPaths, data, cacheTypeEnum);
    }
}
