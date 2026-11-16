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

package org.apache.shenyu.springboot.starter.client.common.config;

import okhttp3.HttpUrl;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.register.ClientApiRefreshedEventListener;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.Environment;

import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Properties;

/**
 * Validates only the built-in configuration owned by an active business-client starter.
 * Extension properties and custom transport address formats remain extension-owned.
 */
public final class ClientRegistrationValidator {

    private static final int MAX_PORT = 65535;

    private static final String DISCOVERY_SERVERS = "shenyu.discovery.serverList";

    private static final String REGISTER_PROPS = "shenyu.register.props.";

    private final RpcTypeEnum protocol;

    public ClientRegistrationValidator(final RpcTypeEnum protocol) {
        this.protocol = protocol;
    }

    /**
     * Validate all bound inputs before calling the resource-owning repository factory.
     *
     * @param register registration transport configuration
     * @param client protocol configuration
     * @param context application context
     * @param validators active starter declarations
     */
    public static void validate(final ShenyuRegisterCenterConfig register, final ShenyuClientConfig client,
                                final ApplicationContext context, final List<ClientRegistrationValidator> validators) {
        final Environment environment = context.getEnvironment();
        final boolean optIn = context.getBeanNamesForType(ClientApiRefreshedEventListener.class, false, false).length > 0;
        required(register.getRegisterType(), "shenyu.register.registerType");
        if ("http".equals(register.getRegisterType())) {
            validateHttpServers(register.getServerLists(), "shenyu.register.serverLists");
        }
        validateDelay(register.getProps(), "shutdownWaitTime");
        validateDelay(register.getProps(), "delayOtherHooksExecTime");
        validateNamespaces(client.getNamespace());
        validateDiscovery(context.getBeanProvider(ShenyuDiscoveryConfig.class).getIfAvailable(), environment);
        validators.forEach(validator -> validator.validateProtocol(client, environment, optIn));
    }

    private static void validateHttpServers(final String servers, final String property) {
        required(servers, property);
        for (String server : servers.split(",", -1)) {
            HttpUrl url;
            try {
                url = HttpUrl.parse(server);
            } catch (IllegalArgumentException ex) {
                throw invalid(property, "comma-separated absolute HTTP/HTTPS URLs with valid ports");
            }
            if (Objects.isNull(url)) {
                throw invalid(property, "comma-separated absolute HTTP/HTTPS URLs with valid ports");
            }
        }
    }

    private static void validateDelay(final Properties properties, final String key) {
        if (Objects.nonNull(properties)) {
            integer(properties.getProperty(key), REGISTER_PROPS + key, 0, Integer.MAX_VALUE);
        }
    }

    private static void validateNamespaces(final String namespace) {
        if (StringUtils.isBlank(namespace)) {
            return;
        }
        for (String entry : namespace.split(Constants.SEPARATOR_CHARS, -1)) {
            if (StringUtils.isBlank(entry)) {
                throw invalid("shenyu.namespace", "nonempty namespace IDs separated by semicolons, or a blank value for the default namespace");
            }
        }
    }

    private static void validateDiscovery(final ShenyuDiscoveryConfig discovery, final Environment environment) {
        if (Objects.isNull(discovery)) {
            return;
        }
        String register = environment.getProperty("shenyu.discovery.register");
        if (Objects.nonNull(register) && !List.of("true", "false").contains(register.toLowerCase(Locale.ROOT))) {
            throw invalid("shenyu.discovery.register", "true or false");
        }
        boolean registration = Boolean.parseBoolean(register);
        if (!registration && Objects.isNull(discovery.getServerList())) {
            return;
        }
        required(discovery.getType(), "shenyu.discovery.type");
        if (Objects.nonNull(discovery.getServerList())) {
            required(discovery.getServerList(), DISCOVERY_SERVERS);
        }
        if (registration) {
            validateDiscoveryProvider(discovery);
        }
    }

    private static void validateDiscoveryProvider(final ShenyuDiscoveryConfig discovery) {
        String type = discovery.getType();
        if (List.of("zookeeper", "nacos", "etcd", "consul", "eureka").contains(type)) {
            required(discovery.getServerList(), DISCOVERY_SERVERS);
        }
        Properties properties = discovery.getProps();
        if (Objects.isNull(properties)) {
            return;
        }
        if ("zookeeper".equals(type)) {
            for (String key : List.of("sessionTimeout", "connectionTimeout", "baseSleepTime", "maxRetries", "maxSleepTime")) {
                integer(properties.getProperty(key), "shenyu.discovery.props." + key, 0, Integer.MAX_VALUE);
            }
        }
        if ("etcd".equals(type)) {
            validateHttpServers(discovery.getServerList(), DISCOVERY_SERVERS);
            integer(properties.getProperty("etcdTimeout"), "shenyu.discovery.props.etcdTimeout", 0, Long.MAX_VALUE);
            integer(properties.getProperty("etcdTTL"), "shenyu.discovery.props.etcdTTL", 1, Long.MAX_VALUE);
        }
    }

    private void validateProtocol(final ShenyuClientConfig client, final Environment environment, final boolean optIn) {
        String prefix = "shenyu.client." + protocol.getName() + ".props.";
        if (Objects.isNull(client.getClient()) || !client.getClient().containsKey(protocol.getName())) {
            throw invalid(prefix.substring(0, prefix.length() - 1), "the property map for the enabled protocol");
        }
        ShenyuClientConfig.ClientPropertiesConfig entry = client.getClient().getOrDefault(protocol.getName(), null);
        if (Objects.isNull(entry) || Objects.isNull(entry.getProps())) {
            throw invalid(prefix.substring(0, prefix.length() - 1), "the property map for the enabled protocol");
        }
        Properties props = entry.getProps();
        validateIdentity(props, prefix, environment, optIn);
        validatePort(props, prefix);
        validateRpcSettings(props, prefix);
    }

    private void validatePort(final Properties props, final String prefix) {
        boolean automatic = protocol == RpcTypeEnum.HTTP || protocol == RpcTypeEnum.WEB_SOCKET;
        integer(props.getProperty(ShenyuClientConstants.PORT), prefix + ShenyuClientConstants.PORT, automatic ? -1 : 1, MAX_PORT);
        if (protocol == RpcTypeEnum.GRPC || protocol == RpcTypeEnum.TARS || protocol == RpcTypeEnum.SOFA) {
            required(props.getProperty(ShenyuClientConstants.PORT), prefix + ShenyuClientConstants.PORT);
        }
    }

    private void validateRpcSettings(final Properties props, final String prefix) {
        if (protocol == RpcTypeEnum.GRPC || protocol == RpcTypeEnum.TARS) {
            required(props.getProperty(ShenyuClientConstants.CONTEXT_PATH), prefix + ShenyuClientConstants.CONTEXT_PATH);
        }
        if (protocol == RpcTypeEnum.GRPC) {
            required(props.getProperty(ShenyuClientConstants.IP_PORT), prefix + ShenyuClientConstants.IP_PORT);
        }
        if (protocol == RpcTypeEnum.TARS) {
            required(props.getProperty(ShenyuClientConstants.HOST), prefix + ShenyuClientConstants.HOST);
        }
    }

    private void validateIdentity(final Properties props, final String prefix, final Environment environment, final boolean optIn) {
        String appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        if (protocol == RpcTypeEnum.HTTP) {
            if (!optIn) {
                appName = StringUtils.defaultIfBlank(appName, environment.getProperty("spring.application.name"));
                required(appName, prefix + ShenyuClientConstants.APP_NAME + " or spring.application.name");
                return;
            }
            if (Objects.isNull(appName)) {
                appName = environment.getProperty("spring.application.name");
            }
        }
        if (StringUtils.isBlank(appName) && StringUtils.isBlank(props.getProperty(ShenyuClientConstants.CONTEXT_PATH))) {
            throw invalid(prefix + ShenyuClientConstants.APP_NAME + " or " + prefix + ShenyuClientConstants.CONTEXT_PATH,
                    "an application name or context path for the enabled protocol");
        }
    }

    private static void integer(final String value, final String property, final long minimum, final long maximum) {
        if (Objects.isNull(value)) {
            return;
        }
        try {
            long parsed = Long.parseLong(value);
            if (parsed >= minimum && parsed <= maximum) {
                return;
            }
        } catch (NumberFormatException ex) {
            // Discard the parser exception: its message contains the supplied value.
        }
        throw invalid(property, "an integer from " + minimum + " to " + maximum);
    }

    private static void required(final String value, final String property) {
        if (StringUtils.isBlank(value)) {
            throw invalid(property, "a nonblank value for the enabled client");
        }
    }

    private static IllegalArgumentException invalid(final String property, final String expected) {
        return new IllegalArgumentException("Invalid client registration property " + property + ": expected " + expected + "; configure this property before enabling registration");
    }
}
