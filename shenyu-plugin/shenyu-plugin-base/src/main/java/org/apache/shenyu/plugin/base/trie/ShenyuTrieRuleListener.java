package org.apache.shenyu.plugin.base.trie;


import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.RuleTrieEventEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.event.RuleTrieEvent;
import org.springframework.context.ApplicationListener;

import java.util.List;
import java.util.stream.Collectors;

public class ShenyuTrieRuleListener implements ApplicationListener<RuleTrieEvent> {

    @Override
    public void onApplicationEvent(RuleTrieEvent event) {
        RuleTrieEventEnum eventEnum = event.getRuleTrieEvent();
        RuleData ruleData = (RuleData) event.getSource();
        List<ConditionData> conditionDataList = ruleData.getConditionDataList();
        List<ConditionData> filterConditions = conditionDataList.stream()
                .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                .collect(Collectors.toList());
        List<String> uriPaths = filterConditions.stream().map(ConditionData::getParamValue).collect(Collectors.toList());
        switch (eventEnum) {
            case INSERT:
                uriPaths.forEach(path -> SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode(path, ruleData, null));
                break;
            case REMOVE:
                uriPaths.forEach(path -> SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).remove(path, ruleData.getSelectorId()));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + event.getRuleTrieEvent());
        }
    }
}
