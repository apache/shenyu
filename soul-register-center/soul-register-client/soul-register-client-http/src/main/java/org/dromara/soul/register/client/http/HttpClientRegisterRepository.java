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

package org.dromara.soul.register.client.http;

import org.dromara.soul.client.common.dto.MetaDataDTO;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;

@ConditionalOnMissingBean(SoulClientRegisterRepository.class)
public class HttpClientRegisterRepository implements SoulClientRegisterRepository {

    private String url;

    @Override
    public void init(final SoulRegisterCenterConfiguration config) {
        url = config.getServerLists();
    }

    @Override
    public void persistInterface(final MetaDataDTO metadata) {
        RegisterUtils.doRegister(GsonUtils.getGson().toJson(metadata), url, RpcTypeEnum.DUBBO);
    }

    @Override
    public void persistInterface(final String key, final String value) {
    
    }
    
    @Override
    public void persistServer(final String key, final String value) {
    
    }
}
