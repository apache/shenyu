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

package org.apache.shenyu.register.client.http;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.enums.RegisterTypeEnum;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The type Http client register repository.
 */
@Join
public class HttpClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(RegisterUtils.class);

    private List<String> serverList;

    private Gson gson = new Gson();

    private Map<String, String> turn = new HashMap<>();

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
        initTurn();
    }

    protected void initTurn() {
        turn.put(RegisterTypeEnum.DUBBO.getName(), "/shenyu-client/dubbo-register");
        turn.put(RegisterTypeEnum.GRPC.getName(), "/shenyu-client/grpc-register");
        turn.put(RegisterTypeEnum.HTTP.getName(), "/shenyu-client/springmvc-register");
        turn.put(RegisterTypeEnum.SOFA.getName(), "/shenyu-client/sofa-register");
        turn.put(RegisterTypeEnum.SPRING_CLOUD.getName(), "/shenyu-client/springcloud-register");
        turn.put(RegisterTypeEnum.TARS.getName(), "/shenyu-client/tars-register");
        turn.put(RegisterTypeEnum.MOTAN.getName(), "/shenyu-client/motan-register");
    }

    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String rpcType = metadata.getRpcType();
        for (String server : serverList) {
            try {
                RegisterUtils.doRegister(gson.toJson(metadata), server + turn.get(rpcType), rpcType);
                return;
            } catch (Exception e) {
                LOGGER.error("register admin url :{} is fail, will retry", server);
            }
        }
    }
}
