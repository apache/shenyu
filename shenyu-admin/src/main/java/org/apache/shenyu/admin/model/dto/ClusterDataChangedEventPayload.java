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

package org.apache.shenyu.admin.model.dto;

import java.io.Serializable;

/**
 * Payload used to hand a committed {@link org.apache.shenyu.admin.listener.DataChangedEvent}
 * over from a non-master admin node to the current master node.
 */
public class ClusterDataChangedEventPayload implements Serializable {

    private static final long serialVersionUID = -6354049813404241267L;

    /**
     * the config group key name, see {@link org.apache.shenyu.common.enums.ConfigGroupEnum}.
     */
    private String groupKey;

    /**
     * the event type name, see {@link org.apache.shenyu.common.enums.DataEventTypeEnum}.
     */
    private String eventType;

    /**
     * the JSON serialized source list of the event.
     */
    private String source;

    public ClusterDataChangedEventPayload() {
    }

    /**
     * Instantiates a new cluster data changed event payload.
     *
     * @param groupKey  the group key name
     * @param eventType the event type name
     * @param source    the JSON serialized source list
     */
    public ClusterDataChangedEventPayload(final String groupKey, final String eventType, final String source) {
        this.groupKey = groupKey;
        this.eventType = eventType;
        this.source = source;
    }

    /**
     * Gets the group key name.
     *
     * @return the group key name
     */
    public String getGroupKey() {
        return groupKey;
    }

    /**
     * Sets the group key name.
     *
     * @param groupKey the group key name
     */
    public void setGroupKey(final String groupKey) {
        this.groupKey = groupKey;
    }

    /**
     * Gets the event type name.
     *
     * @return the event type name
     */
    public String getEventType() {
        return eventType;
    }

    /**
     * Sets the event type name.
     *
     * @param eventType the event type name
     */
    public void setEventType(final String eventType) {
        this.eventType = eventType;
    }

    /**
     * Gets the JSON serialized source list.
     *
     * @return the JSON serialized source list
     */
    public String getSource() {
        return source;
    }

    /**
     * Sets the JSON serialized source list.
     *
     * @param source the JSON serialized source list
     */
    public void setSource(final String source) {
        this.source = source;
    }
}
