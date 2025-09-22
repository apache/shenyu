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

package org.apache.shenyu.plugin.motan.handler;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.motan.cache.ApplicationConfigCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

/**
 * The motan metadata handler.
 */
public class MotanMetaDataHandler implements MetaDataHandler {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(MotanMetaDataHandler.class);
    
    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();
    
    @Override
    public void handle(final MetaData metaData) {
        try {
            MetaData exist = META_DATA.get(metaData.getPath());
            if (Objects.isNull(exist) || Objects.isNull(ApplicationConfigCache.getInstance().get(exist.getPath()))
                    || Objects.isNull(ApplicationConfigCache.getInstance().get(exist.getPath()).getRef())) {
                // The first initialization
                ApplicationConfigCache.getInstance().initRef(metaData);
            } else {
                if (!exist.getServiceName().equals(metaData.getServiceName()) || !exist.getRpcExt().equals(metaData.getRpcExt())) {
                    // update
                    ApplicationConfigCache.getInstance().invalidateWithMetadataPath(metaData.getPath());
                    ApplicationConfigCache.getInstance().build(metaData);
                }
            }
            META_DATA.put(metaData.getPath(), metaData);
        } catch (Exception e) {
            LOG.error("motan sync metadata is error, please check motan service. MetaData: [{}]", metaData, e);
        }
    }
    
    @Override
    public void remove(final MetaData metaData) {
        ApplicationConfigCache.getInstance().invalidateWithMetadataPath(metaData.getPath());
        META_DATA.remove(metaData.getPath());
    }
    
    @Override
    public String rpcType() {
        return RpcTypeEnum.MOTAN.getName();
    }
}
