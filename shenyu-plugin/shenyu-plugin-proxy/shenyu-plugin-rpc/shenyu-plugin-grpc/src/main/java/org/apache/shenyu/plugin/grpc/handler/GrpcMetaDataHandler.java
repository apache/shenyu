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

package org.apache.shenyu.plugin.grpc.handler;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;

/**
 * The type grpc meta data handler.
 */
public class GrpcMetaDataHandler implements MetaDataHandler {

    @Override
    public void handle(final MetaData metaData) {
        ApplicationConfigCache.getInstance().cacheGrpcExtInfo(metaData);
    }

    @Override
    public void remove(final MetaData metaData) {
        ApplicationConfigCache.getInstance().invalidateGrpcExtInfo(metaData);
    }

    @Override
    public void refresh() {
        ApplicationConfigCache.getInstance().cleanGrpcExtInfo();
    }

    @Override
    public String rpcType() {
        return RpcTypeEnum.GRPC.getName();
    }
}
