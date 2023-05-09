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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.Environment;

import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

public class ClientRegisterConfigImpl implements ClientRegisterConfig {

    private final Properties props;

    private Integer port;

    private String host;

    private String appName;

    private final Environment env;

    private final ApplicationContext applicationContext;

    private final RpcTypeEnum rpcTypeEnum;

    public ClientRegisterConfigImpl(final ShenyuClientConfig shenyuClientConfig,
                                    final RpcTypeEnum rpcTypeEnum,
                                    final ApplicationContext applicationContext,
                                    final Environment env) {

        this.props = shenyuClientConfig.getClient().get(rpcTypeEnum.getName()).getProps();

        this.applicationContext = applicationContext;

        this.rpcTypeEnum = rpcTypeEnum;

        this.env = env;

    }

    @Override
    public Integer getPort() {

        if (Objects.isNull(port)) {
            this.port = Optional
                    .ofNullable(props.getProperty(ShenyuClientConstants.PORT))
                    .map(Integer::parseInt)
                    .orElseGet(() -> PortUtils
                            .findPort(applicationContext.getAutowireCapableBeanFactory()));
        }

        return this.port;
    }

    @Override
    public String getHost() {

        if (Objects.isNull(this.host)) {

            this.host = props.getProperty(ShenyuClientConstants.HOST);

            this.host = IpUtils.isCompleteHost(this.host) ? this.host
                    : IpUtils.getHost(this.host);
        }

        return this.host;
    }

    @Override
    public String getAppName() {

        if (Objects.isNull(this.appName)) {

            this.appName = Optional.ofNullable(props.getProperty(ShenyuClientConstants.APP_NAME))
                    .orElseGet(() -> env.getProperty("spring.application.name"));
        }

        return this.appName;
    }

    @Override
    public String getContextPath() {
        return Optional
                .ofNullable(props.getProperty(ShenyuClientConstants.CONTEXT_PATH))
                .map(UriUtils::repairData).orElse("");
    }

    @Override
    public String getIpAndPort() {
        return props.getProperty(ShenyuClientConstants.IP_PORT);
    }

    @Override
    public Boolean getAddPrefixed() {
        return Optional.ofNullable(props.getProperty(ShenyuClientConstants.ADD_PREFIXED))
                .map(Boolean::parseBoolean).orElse(false);
    }

    @Override
    public RpcTypeEnum getRpcTypeEnum() {
        return rpcTypeEnum;
    }
}
