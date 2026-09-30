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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.enums.DataEventTypeEnum;

/**
 * The type Abstract data handler.
 *
 * @param <T> the type parameter
 */
public abstract class AbstractDataHandler<T> implements DataHandler {

    /**
     * Convert list.
     *
     * @param json the json
     * @return the list
     */
    protected abstract List<T> convert(String json);

    /**
     * Do refresh.
     *
     * @param dataList the data list
     */
    protected abstract void doRefresh(List<T> dataList);

    /**
     * Do update.
     *
     * @param dataList the data list
     */
    protected abstract void doUpdate(List<T> dataList);

    /**
     * Do delete.
     *
     * @param dataList the data list
     */
    protected abstract void doDelete(List<T> dataList);

    /**
     * Apply an authoritative snapshot for the connection's namespace.
     * @param json snapshot array, including an empty array
     */
    public void handleSnapshot(final String json) {
        List<T> dataList = convert(json);
        if (java.util.Objects.isNull(dataList)) {
            throw new IllegalArgumentException("A snapshot must contain a data array");
        }
        doSnapshot(dataList);
    }

    /**
     * Replace the complete group, not just rows present in the payload.
     * @param dataList complete group
     */
    protected void doSnapshot(final List<T> dataList) {
        doRefresh(dataList);
    }

    @Override
    public void handle(final String json, final String eventType) {
        List<T> dataList = convert(json);
        if (CollectionUtils.isEmpty(dataList)) {
            return;
        }

        DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
        switch (eventTypeEnum) {
            case REFRESH:
            case MYSELF:
                doRefresh(dataList);
                break;
            case UPDATE:
            case CREATE:
                doUpdate(dataList);
                break;
            case DELETE:
                doDelete(dataList);
                break;
            default:
                break;
        }
    }
}
