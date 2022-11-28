package org.apache.shenyu.plugin.base.event;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.RuleTrieEventEnum;
import org.springframework.context.ApplicationEvent;

/**
 * Rule trie event.
 */
public class RuleTrieEvent extends ApplicationEvent {

    private static final long serialVersionUID = -6616858497711197175L;

    private final RuleTrieEventEnum ruleTrieEventEnum;

    /**
     * shenyu trie event.
     *
     * @param ruleTrieEventEnum ruleTrieEventEnum
     * @param source data
     */
    public RuleTrieEvent(final RuleTrieEventEnum ruleTrieEventEnum, final RuleData source) {
        super(source);
        this.ruleTrieEventEnum = ruleTrieEventEnum;
    }

    /**
     * get rule trie build or remove event.
     *
     * @return {@linkplain RuleTrieEventEnum} include insert and remove event
     */
    public RuleTrieEventEnum getRuleTrieEvent() {
        return ruleTrieEventEnum;
    }
}
