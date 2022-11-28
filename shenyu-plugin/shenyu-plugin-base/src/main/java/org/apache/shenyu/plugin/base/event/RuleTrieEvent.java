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
