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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.GrpcRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.cache.GrpcClientCache;

import java.util.Objects;

/**
 * The type Grpc plugin data handler.
 */
public class GrpcPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && Boolean.TRUE.equals(pluginData.getEnabled())) {
            GrpcRegisterConfig grpcRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), GrpcRegisterConfig.class);
            GrpcRegisterConfig exist = Singleton.INST.get(GrpcRegisterConfig.class);
            if (Objects.isNull(grpcRegisterConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !grpcRegisterConfig.equals(exist)) {
                // If it is null, cache it
                Singleton.INST.single(GrpcRegisterConfig.class, grpcRegisterConfig);
            }
        }
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData.getName())) {
            return;
        }
        GrpcClientCache.initGrpcClient(selectorData.getName());
        ApplicationConfigCache.getInstance().initPrx(selectorData);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        if (Objects.isNull(selectorData.getName())) {
            return;
        }
        ApplicationConfigCache.getInstance().invalidate(selectorData.getName());
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.GRPC.getName();
    }
}
