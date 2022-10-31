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

import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;

/**
 * SelectorUpdatedEvent.
 */
public class RuleUpdatedEvent extends RuleChangedEvent {
    
    
    /**
     * Create a new {@code RuleChangedEvent}.operator is unknown.
     *
     * @param source   Current rule state
     * @param before   before rule state
     * @param operator operator
     */
    public RuleUpdatedEvent(final RuleDO source, final RuleDO before, final String operator) {
        super(source, before, EventTypeEnum.RULE_UPDATE, operator);
    }
    
    /**
     * the created selector.
     *
     * @return selector
     */
    public RuleDO getRule() {
        return (RuleDO) getSource();
    }
    
}
