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

package org.apache.shenyu.admin.model.event;

import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.springframework.context.ApplicationEvent;

import java.util.Date;
import java.util.Objects;

/**
 * AdminDataModelChangedEvent.
 */
public class AdminDataModelChangedEvent extends ApplicationEvent {
    
    /**
     * action type.
     */
    private final EventTypeEnum type;
    
    /**
     * before data.
     */
    private final Object before;
    
    /**
     * after data.
     */
    private final Object after;
    
    /**
     * operator:is user or app.
     */
    private final String operator;
    
    /**
     * consumed.
     */
    private boolean consumed;
    
    /**
     * event date.
     */
    private final Date date;
    
    /**
     * Create a new {@code ApplicationEvent}.operator is unknown.
     *
     * @param source Current data state
     * @param before Before the change data state
     * @param type   event type
     */
    public AdminDataModelChangedEvent(final Object source, final Object before, final EventTypeEnum type) {
        this(source, before, type, null);
    }
    
    /**
     * Create a new {@code ApplicationEvent}.
     *
     * @param source   Current data state
     * @param before   Before the change data state
     * @param type     event type
     * @param operator operator,default is unknown
     */
    public AdminDataModelChangedEvent(final Object source, final Object before, final EventTypeEnum type, final String operator) {
        super(source);
        this.type = type;
        this.before = before;
        this.after = source;
        this.operator = Objects.isNull(operator) ? "unknown" : operator;
        this.consumed = false;
        this.date = new Date();
    }
    
    
    /**
     * get type.
     *
     * @return type
     */
    public EventTypeEnum getType() {
        return type;
    }
    
    /**
     * get before.
     *
     * @return before data
     */
    public Object getBefore() {
        return before;
    }
    
    /**
     * get after.
     *
     * @return after data
     */
    public Object getAfter() {
        return after;
    }
    
    /**
     * get operator.
     *
     * @return operator
     */
    public String getOperator() {
        return operator;
    }
    
    /**
     * consumed.
     */
    public void consumed() {
        this.consumed = true;
    }
    
    /**
     * is consumed.
     *
     * @return is consumed
     */
    public boolean isConsumed() {
        return consumed;
    }
    
    
    /**
     * get date.
     *
     * @return event date
     */
    public Date getDate() {
        return date;
    }
    
    /**
     * before data snapshot.
     *
     * @return snapshot
     */
    public String beforeSnapshot() {
        return Objects.toString(before, "before unknown");
    }
    
    /**
     * after data snapshot.
     *
     * @return snapshot
     */
    public String afterSnapshot() {
        return Objects.toString(after, "after unknown");
    }
    
    /**
     * build event context.
     *
     * @return event context
     */
    public String buildContext() {
        return String.format("%s changed(%s)[%s = > %s]", eventName(), type.getTypeName(), beforeSnapshot(), afterSnapshot());
    }
    
    /**
     * event name.
     *
     * @return name
     */
    public String eventName() {
        return "data";
    }
}
