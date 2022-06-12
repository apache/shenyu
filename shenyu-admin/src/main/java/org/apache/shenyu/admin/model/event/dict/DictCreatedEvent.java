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

package org.apache.shenyu.admin.model.event.dict;

import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;

/**
 * DictCreatedEvent.
 */
public class DictCreatedEvent extends DictChangedEvent {
    
    
    /**
     * Create a new {@code DictCreatedEvent}.operator is unknown.
     *
     * @param source   Current dict state
     * @param operator operator
     */
    public DictCreatedEvent(final ShenyuDictDO source, final String operator) {
        super(source, null, EventTypeEnum.DICT_CREATE, operator);
    }
    
    /**
     * the created dict.
     *
     * @return dict
     */
    public ShenyuDictDO getDict() {
        return (ShenyuDictDO) getSource();
    }
    
}
