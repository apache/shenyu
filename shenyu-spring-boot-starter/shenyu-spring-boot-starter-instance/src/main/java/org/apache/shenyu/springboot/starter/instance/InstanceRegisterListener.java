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

package org.apache.shenyu.springboot.starter.instance;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig.InstanceConfig;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.core.ShenyuInstanceRegisterRepositoryFactory;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.ApplicationListener;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Instance register listener.
 */
public class InstanceRegisterListener implements ApplicationListener<WebServerInitializedEvent> {
    
    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    private final String host;
    
    private final String appName;
    
    private final Properties props;
    
    private final ShenyuInstanceRegisterRepository repository;
    
    /**
     * Instantiates a new Instance register listener.
     *
     * @param config the config
     */
    public InstanceRegisterListener(final InstanceConfig config) {
        String registerType = config.getRegisterType();
        String serverLists = config.getServerLists();
        if (StringUtils.isBlank(registerType) || StringUtils.isBlank(serverLists)) {
            throw new ShenyuException("please config the registerType and serverList");
        }
        repository = ShenyuInstanceRegisterRepositoryFactory.newInstance(config.getRegisterType());
        repository.init(config);
        this.props = config.getProps();
        String name = props.getProperty("name");
        this.appName = StringUtils.isBlank(name) ? "shenyu-gateway" : name;
        String host = props.getProperty("host");
        this.host = StringUtils.isBlank(host) ? IpUtils.getHost() : host;
    }
    
    @Override
    public void onApplicationEvent(final WebServerInitializedEvent event) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        String configPort = props.getProperty("port");
        int port = StringUtils.isBlank(configPort) ? event.getWebServer().getPort() : Integer.parseInt(configPort);
        InstanceRegisterDTO registerDTO = buildInstanceRegisterDTO(port);
        repository.persistInstance(registerDTO);
    }
    
    private InstanceRegisterDTO buildInstanceRegisterDTO(final int port) {
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        return InstanceRegisterDTO.builder()
                .appName(appName)
                .host(host)
                .port(port)
                .build();
    }
}
