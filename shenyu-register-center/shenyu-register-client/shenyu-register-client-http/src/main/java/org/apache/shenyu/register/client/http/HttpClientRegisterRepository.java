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

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.AesUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;
import org.apache.shenyu.register.client.http.utils.RegisterUtils;
import org.apache.shenyu.register.client.http.utils.RuntimeUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.spi.Join;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * The type Http client register repository.
 */
@Join
public class HttpClientRegisterRepository extends FailbackRegistryRepository {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpClientRegisterRepository.class);

    private static URIRegisterDTO uriRegisterDTO;

    private static ApiDocRegisterDTO apiDocRegisterDTO;

    private String username;
    
    private String password;
    
    private List<String> serverList;
    
    /**
     * server -> accessToken.
     */
    private LoadingCache<String, String> accessToken;
    
    /**
     * Instantiates a new Http client register repository.
     */
    public HttpClientRegisterRepository() {
    }
    
    /**
     * Instantiates a new Http client register repository.
     *
     * @param config the config
     */
    public HttpClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }
    
    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        this.username = config.getProps().getProperty(Constants.USER_NAME);
        this.password = config.getProps().getProperty(Constants.PASS_WORD);
        String secretKey = config.getProps().getProperty(Constants.AES_SECRET_KEY);
        String secretIv = config.getProps().getProperty(Constants.AES_SECRET_IV);
        if (StringUtils.isNotBlank(secretKey) && StringUtils.isNotBlank(secretIv)) {
            this.password = AesUtils.cbcEncrypt(secretKey, secretIv, password);
        }
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
        this.accessToken = Caffeine.newBuilder()
                //see org.apache.shenyu.admin.config.properties.JwtProperties#expiredSeconds
                .expireAfterWrite(24L, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<?> login = RegisterUtils.doLogin(username, password, server.concat(Constants.LOGIN_PATH));
                            return login.map(String::valueOf).orElse(null);
                        } catch (Exception e) {
                            LOGGER.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void doPersistURI(final URIRegisterDTO registerDTO) {
        if (RuntimeUtils.listenByOther(registerDTO.getPort())) {
            return;
        }
        doRegister(registerDTO, Constants.URI_PATH, Constants.URI);
        uriRegisterDTO = registerDTO;
    }
    
    @Override
    public void offline(final URIRegisterDTO offlineDTO) {
        doUnregister(offlineDTO);
    }
    
    @Override
    public void sendHeartbeat(final URIRegisterDTO heartbeatDTO) {
        if (RuntimeUtils.listenByOther(heartbeatDTO.getPort())) {
            return;
        }
        heartbeatDTO.setEventType(EventType.HEARTBEAT);
        doHeartbeat(heartbeatDTO, Constants.URI_PATH);
    }
    
    /**
     * doPersistApiDoc.
     *
     * @param registerDTO registerDTO
     */
    @Override
    protected void doPersistApiDoc(final ApiDocRegisterDTO registerDTO) {
        doRegister(registerDTO, Constants.API_DOC_PATH, Constants.API_DOC_TYPE);
        apiDocRegisterDTO = registerDTO;
    }
    
    @Override
    public void doPersistInterface(final MetaDataRegisterDTO metadata) {
        doRegister(metadata, Constants.META_PATH, Constants.META_TYPE);
    }

    @Override
    public void closeRepository() {
        if (Objects.nonNull(uriRegisterDTO)) {
            uriRegisterDTO.setEventType(EventType.DELETED);
            doRegister(uriRegisterDTO, Constants.URI_PATH, Constants.URI);
        }
        if (Objects.nonNull(apiDocRegisterDTO)) {
            apiDocRegisterDTO.setEventType(EventType.OFFLINE);
            doRegister(apiDocRegisterDTO, Constants.API_DOC_PATH, Constants.API_DOC_TYPE);
        }
    }

    /**
     * doPersistDiscoveryConfig.
     *
     * @param discoveryConfigRegisterDTO discoveryConfigRegisterDTO
     */
    public void doPersistDiscoveryConfig(final DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO) {
        doRegister(discoveryConfigRegisterDTO, Constants.DISCOVERY_CONFIG_PATH, Constants.DISCOVERY_CONFIG_TYPE);
    }

    private <T> void doRegister(final T t, final String path, final String type) {
        int i = 0;
        for (String server : serverList) {
            i++;
            String concat = server.concat(path);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doRegister(GsonUtils.getInstance().toJson(t), concat, type, accessToken);
                // considering the situation of multiple clusters, we should continue to execute here
            } catch (Exception e) {
                LOGGER.error("Register admin url :{} is fail, will retry. cause:{}", server, e.getMessage());
                if (i == serverList.size()) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    private <T> void doHeartbeat(final T t, final String path) {
        int i = 0;
        for (String server : serverList) {
            i++;
            String concat = server.concat(path);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doHeartBeat(GsonUtils.getInstance().toJson(t), concat, Constants.HEARTBEAT, accessToken);
            } catch (Exception e) {
                LOGGER.error("HeartBeat admin url :{} is fail, will retry.", server, e);
                if (i == serverList.size()) {
                    throw new RuntimeException(e);
                }
            }
        }
    }
    
    private <T> void doUnregister(final T t) {
        for (String server : serverList) {
            String concat = server.concat(Constants.OFFLINE_PATH);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doUnregister(GsonUtils.getInstance().toJson(t), concat, accessToken);
                // considering the situation of multiple clusters, we should continue to execute here
            } catch (Exception e) {
                LOGGER.error("Unregister admin url :{} is fail. cause:{}", server, e.getMessage());
            }
        }
    }
}
