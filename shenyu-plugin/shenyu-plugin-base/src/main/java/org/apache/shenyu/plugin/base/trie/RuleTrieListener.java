package org.apache.shenyu.plugin.base.trie;


import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.RuleTrieEventEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.event.RuleTrieEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class RuleTrieListener implements ApplicationListener<RuleTrieEvent> {

    @Override
    public void onApplicationEvent(RuleTrieEvent event) {
        RuleTrieEventEnum eventEnum = event.getRuleTrieEvent();
        RuleData ruleData = (RuleData) event.getSource();
        List<ConditionData> conditionDataList = ruleData.getConditionDataList();
        List<ConditionData> filterConditions = conditionDataList.stream()
                .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                .collect(Collectors.toList());
        switch (eventEnum) {
            case INSERT:
                filterConditions.forEach(conditionData -> {
                    SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode(conditionData.getParamValue(), ruleData, ruleData);
                });
                break;
            case REMOVE:
                filterConditions.forEach(conditionData -> {
                    SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).remove(conditionData.getParamValue());
                });
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + event.getRuleTrieEvent());
        }


    }
}
