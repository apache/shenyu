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

package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;

/**
 * The type Plugin name adapter.
 */
public class PluginNameAdapter {
    
    /**
     * Rpc type adapter string.
     *
     * @param rpcType the rpc type
     * @return the string
     */
    public static String rpcTypeAdapter(final String rpcType) {
        RpcTypeEnum rpcTypeEnum = RpcTypeEnum.acquireByName(rpcType);
        switch (rpcTypeEnum) {
            case GRPC:
                return PluginEnum.GRPC.getName();
            case SPRING_CLOUD:
                return PluginEnum.SPRING_CLOUD.getName();
            case DUBBO:
                return PluginEnum.DUBBO.getName();
            case TARS:
                return PluginEnum.TARS.getName();
            case SOFA:
                return PluginEnum.SOFA.getName();
            case WEB_SOCKET:
                return PluginEnum.WEB_SOCKET.getName();
            case MOTAN:
                return PluginEnum.MOTAN.getName();
            case HTTP:
            default:
                return PluginEnum.DIVIDE.getName();
        }
    }
}
