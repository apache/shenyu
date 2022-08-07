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

package org.apache.shenyu.plugin.base.cache;

import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The type common meta data subscriber.
 */
public class CommonMetaDataSubscriber implements MetaDataSubscriber {

    private final Map<String, MetaDataHandler> handlerMap;

    /**
     * Instantiates a new Common meta data subscriber.
     *
     * @param metaDataHandlerList the plugin data handler list
     */
    public CommonMetaDataSubscriber(final List<MetaDataHandler> metaDataHandlerList) {
        this.handlerMap = metaDataHandlerList.stream().collect(Collectors.toConcurrentMap(MetaDataHandler::rpcType, e -> e));
    }

    @Override
    public void onSubscribe(final MetaData metaData) {
        Optional.ofNullable(handlerMap.get(metaData.getRpcType()))
                .ifPresent(handler -> handler.handle(metaData));
    }

    @Override
    public void unSubscribe(final MetaData metaData) {
        Optional.ofNullable(handlerMap.get(metaData.getRpcType()))
                .ifPresent(handler -> handler.remove(metaData));
    }

    @Override
    public void refresh() {
        if (MapUtils.isEmpty(handlerMap)) {
            return;
        }
        handlerMap.forEach((k, v) -> v.refresh());
    }
}
