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

package org.apache.shenyu.plugin.apache.dubbo.subscriber;

import com.google.common.collect.Maps;

import java.util.Objects;
import java.util.concurrent.ConcurrentMap;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApplicationConfigCache;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;

/**
 * The type Apache dubbo meta data subscriber.
 */
public class ApacheDubboMetaDataSubscriber implements MetaDataSubscriber {

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    @Override
    public void onSubscribe(final MetaData metaData) {
        if (RpcTypeEnum.DUBBO.getName().equals(metaData.getRpcType())) {
            MetaData exist = META_DATA.get(metaData.getPath());
            if (Objects.isNull(exist) || Objects.isNull(ApplicationConfigCache.getInstance().get(metaData.getPath()))) {
                // The first initialization
                ApplicationConfigCache.getInstance().initRef(metaData);
            } else {
                // There are updates, which only support the update of four properties of serviceName rpcExt parameterTypes methodName,
                // because these four properties will affect the call of Dubbo;
                if (!Objects.equals(metaData.getServiceName(), exist.getServiceName())
                        || !Objects.equals(metaData.getRpcExt(), exist.getRpcExt())
                        || !Objects.equals(metaData.getParameterTypes(), exist.getParameterTypes())
                        || !Objects.equals(metaData.getMethodName(), exist.getMethodName())) {
                    ApplicationConfigCache.getInstance().build(metaData);
                }
            }
            META_DATA.put(metaData.getPath(), metaData);
        }
    }

    @Override
    public void unSubscribe(final MetaData metaData) {
        if (RpcTypeEnum.DUBBO.getName().equals(metaData.getRpcType())) {
            ApplicationConfigCache.getInstance().invalidate(metaData.getPath());
            META_DATA.remove(metaData.getPath());
        }
    }
}
