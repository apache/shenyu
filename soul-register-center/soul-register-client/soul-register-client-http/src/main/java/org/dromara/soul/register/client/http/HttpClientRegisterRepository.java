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

import com.google.gson.Gson;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.client.http.utils.RegisterUtils;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataDTO;
import org.dromara.soul.register.common.enums.RegisterTypeEnum;
import org.dromara.soul.spi.Join;

/**
 * The type Http client register repository.
 *
 * @author xiaoyu
 */
@Join
public class HttpClientRegisterRepository implements SoulClientRegisterRepository {

    private String url;
    
    private Gson gson = new Gson();

    @Override
    public void init(final SoulRegisterCenterConfig config) {
        url = config.getServerLists();
    }

    @Override
    public void persistInterface(final MetaDataDTO metadata) {
        try {
            if (metadata.getRpcType().equals(RegisterTypeEnum.DUBBO.getName())) {
                RegisterUtils.doRegister(gson.toJson(metadata), url + "/soul-client/dubbo-register", metadata.getRpcType());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
