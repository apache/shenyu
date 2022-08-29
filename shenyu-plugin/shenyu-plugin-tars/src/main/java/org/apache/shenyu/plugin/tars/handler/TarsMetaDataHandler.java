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

package org.apache.shenyu.plugin.tars.handler;

import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.tars.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.tars.proxy.TarsInvokePrx;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * The tars metadata handler.
 */
public class TarsMetaDataHandler implements MetaDataHandler {

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();
    
    @Override
    public void handle(final MetaData metaData) {
        metaData.updateContextPath();
        MetaData metaExist = META_DATA.get(metaData.getPath());
        List<TarsInvokePrx> prxList = ApplicationConfigCache.getInstance()
                .get(metaData.getPath()).getTarsInvokePrxList();
        boolean exist = prxList.stream().anyMatch(tarsInvokePrx -> tarsInvokePrx.getHost().equals(metaData.getAppName()));
        if (!exist) {
            ApplicationConfigCache.getInstance().initPrx(metaData);
        }
        if (Objects.isNull(metaExist)) {
            META_DATA.put(metaData.getPath(), metaData);
        }
    }
    
    @Override
    public void remove(final MetaData metaData) {
        metaData.updateContextPath();
        List<TarsInvokePrx> prxList = ApplicationConfigCache.getInstance()
                .get(metaData.getPath()).getTarsInvokePrxList();
        List<TarsInvokePrx> removePrxList = prxList.stream()
                .filter(tarsInvokePrx -> tarsInvokePrx.getHost().equals(metaData.getAppName()))
                .collect(Collectors.toList());
        prxList.removeAll(removePrxList);
        if (CollectionUtils.isEmpty(prxList)) {
            META_DATA.remove(metaData.getPath());
        }
    }
    
    @Override
    public String rpcType() {
        return RpcTypeEnum.TARS.getName();
    }
}
