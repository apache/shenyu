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

package org.dromara.soul.admin.register.zookeeper;

import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.common.enums.RpcTypeEnum;

import java.util.HashMap;
import java.util.Map;

/**
 * rpc type convert register dto.
 *
 * @author lw1243925457
 */
class RpcTypeTurned {

    private Map<String, Class> convertMap = new HashMap<>();

    RpcTypeTurned() {
        convertMap.put(RpcTypeEnum.HTTP.getName(), SpringMvcRegisterDTO.class);
        convertMap.put(RpcTypeEnum.SPRING_CLOUD.getName(), SpringCloudRegisterDTO.class);
        convertMap.put(RpcTypeEnum.DUBBO.getName(), MetaDataDTO.class);
        convertMap.put(RpcTypeEnum.SOFA.getName(), MetaDataDTO.class);
        convertMap.put(RpcTypeEnum.TARS.getName(), MetaDataDTO.class);
    }

    Class getClass(final String type) {
        return convertMap.get(type);
    }
}
