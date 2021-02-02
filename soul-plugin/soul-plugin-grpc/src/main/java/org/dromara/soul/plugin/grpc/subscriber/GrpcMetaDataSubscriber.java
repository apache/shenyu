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

package org.dromara.soul.plugin.grpc.subscriber;

import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.plugin.grpc.cache.ApplicationConfigCache;
import org.dromara.soul.plugin.grpc.cache.GrpcClientCache;
import org.dromara.soul.plugin.grpc.resolver.SoulServiceInstance;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentMap;
import java.util.stream.Collectors;

/**
 * The grpc metadata subscribe.
 *
 * @author zhanglei
 */
public class GrpcMetaDataSubscriber implements MetaDataSubscriber {

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    @Override
    public void onSubscribe(final MetaData metaData) {
        if (RpcTypeEnum.GRPC.getName().equals(metaData.getRpcType())) {
            if (metaData.getContextPath() == null) {
                return;
            }
            MetaData metaExist = META_DATA.get(metaData.getPath());
            List<SoulServiceInstance> prxList = ApplicationConfigCache.getInstance()
                    .get(metaData.getContextPath()).getSoulServiceInstances();
            if (CollectionUtils.isEmpty(prxList)) {
                GrpcClientCache.initGrpcClient(metaData.getContextPath());
            }
            boolean exist = prxList.stream().anyMatch(instance -> isEqual(instance, metaData.getAppName()));
            if (!exist) {
                ApplicationConfigCache.getInstance().initPrx(metaData);
            }
            if (Objects.isNull(metaExist)) {
                META_DATA.put(metaData.getPath(), metaData);
            }
        }
    }

    @Override
    public void unSubscribe(final MetaData metaData) {
        if (RpcTypeEnum.GRPC.getName().equals(metaData.getRpcType())) {
            List<SoulServiceInstance> prxList = ApplicationConfigCache.getInstance()
                    .get(metaData.getPath()).getSoulServiceInstances();
            List<SoulServiceInstance> removePrxList = prxList.stream()
                    .filter(instance -> isEqual(instance, metaData.getAppName()))
                    .collect(Collectors.toList());
            prxList.removeAll(removePrxList);
            if (CollectionUtils.isEmpty(prxList)) {
                META_DATA.remove(metaData.getPath());
                ApplicationConfigCache.getInstance().invalidate(metaData);
            }
        }
    }

    private Boolean isEqual(final SoulServiceInstance instance, final String appName) {
        String url = String.join(":", instance.getHost(), String.valueOf(instance.getPort()));
        return url.equals(appName);
    }
}
