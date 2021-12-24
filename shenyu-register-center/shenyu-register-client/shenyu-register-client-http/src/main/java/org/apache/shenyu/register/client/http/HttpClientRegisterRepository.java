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
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * The type Http client register repository.
 */
@Join
public class HttpClientRegisterRepository implements ShenyuClientRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(RegisterUtils.class);
    
    private static final String META_PATH = "/shenyu-client/register-metadata";

    private static final String META_TYPE = "metadata";

    private static final String URI_PATH = "/shenyu-client/register-uri";

    private List<String> serverList;

    public HttpClientRegisterRepository() { }

    public HttpClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }

    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        doRegister(registerDTO, URI_PATH, Constants.URI);
    }
    
    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        doRegister(metadata, META_PATH, META_TYPE);
    }
    
    private <T> void doRegister(final T t, final String path, final String type) {
        for (String server : serverList) {
            try {
                RegisterUtils.doRegister(GsonUtils.getInstance().toJson(t), server + path, type);
                return;
            } catch (Exception e) {
                LOGGER.error("register admin url :{} is fail, will retry, ex is :{}", server, e);
            }
        }
    }
}
