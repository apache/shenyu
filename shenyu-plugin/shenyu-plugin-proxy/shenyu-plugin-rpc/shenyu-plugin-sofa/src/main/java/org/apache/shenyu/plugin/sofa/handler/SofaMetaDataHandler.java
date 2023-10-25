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

package org.apache.shenyu.plugin.sofa.handler;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;

import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

/**
 * The sofa metadata handler.
 */
public class SofaMetaDataHandler implements MetaDataHandler {

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();
    
    @Override
    public void handle(final MetaData metaData) {
        MetaData exist = META_DATA.get(metaData.getPath());
        if (Objects.isNull(exist) || Objects.isNull(ApplicationConfigCache.getInstance().get(exist.getPath()).refer())) {
            // The first initialization
            ApplicationConfigCache.getInstance().initRef(metaData);
        } else {
            if (!exist.getServiceName().equals(metaData.getServiceName()) || !exist.getRpcExt().equals(metaData.getRpcExt())) {
                // update
                ApplicationConfigCache.getInstance().build(metaData);
            }
        }
        META_DATA.put(metaData.getPath(), metaData);
    }
    
    @Override
    public void remove(final MetaData metaData) {
        ApplicationConfigCache.getInstance().invalidate(metaData.getPath());
        META_DATA.remove(metaData.getPath());
    }
    
    @Override
    public String rpcType() {
        return RpcTypeEnum.SOFA.getName();
    }
}
