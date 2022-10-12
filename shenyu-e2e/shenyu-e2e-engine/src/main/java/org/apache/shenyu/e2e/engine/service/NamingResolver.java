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

package org.apache.shenyu.e2e.engine.service;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.HostConfigure.HostServiceConfigure;
import org.testcontainers.containers.DockerComposeContainer;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
public enum NamingResolver {
    INSTANCE;
    
    private Map<String, String> namingMap;
    
    public void ofHostConfigure(List<HostServiceConfigure> serviceConfigures) {
        namingMap = serviceConfigures.stream()
                .collect(Collectors.toMap(
                                HostServiceConfigure::getServiceName,
                                c -> getAddressFromBaseUrl(c.getBaseUrl())
                        )
                );
    }
    
    public void ofDockerConfigure(DockerComposeContainer<?> container) {
        final Map<String, String> _namingMap = Maps.newHashMap();
        try {
            Field field = container.getClass().getDeclaredField("serviceInstanceMap");
            field.setAccessible(true);
            Map<String, ?> serviceInstanceMap = (Map<String, ?>) field.get(container);
            
            serviceInstanceMap.keySet().forEach(e -> {
                if (container.getContainerByServiceName(e).isPresent()) {
                    container.getContainerByServiceName(e).get()
                            .getContainerInfo()
                            .getNetworkSettings()
                            .getNetworks()
                            .entrySet()
                            .stream()
                            .findFirst()
                            .ifPresent(net -> {
                                String ip = net.getValue().getIpAddress();
                                Objects.requireNonNull(net.getValue().getAliases()).forEach(alias -> _namingMap.put(alias, ip));
                            });
                } else {
                    log.warn("service {} not exists", e);
                }
            });
            System.out.println(_namingMap);
        } catch (NoSuchFieldException | IllegalAccessException ignore) {
        }
        this.namingMap = ImmutableMap.<String, String>builder().putAll(_namingMap).build();
    }
    
    private static String getAddressFromBaseUrl(String baseUrl) {
        String address = baseUrl.replaceFirst("http(s)*://", "");
        if (address.contains("/")) {
            int index = address.indexOf('/');
            address = address.substring(0, index);
        }
        return address;
    }
    
    /**
     * resolve name to ip address.
     *
     * @param name represents serviceName, hostname, or domain.
     * @return return resulted ip if resolved success. otherwise, return name.
     */
    public String resolve(String name) {
        String rst = namingMap.get(name);
        if (!Strings.isNullOrEmpty(rst)) {
            log.info("resolved {} to {} by docker-compose", name, rst);
            return rst;
        }
        
        try {
            InetAddress address = InetAddress.getByName(name);
            rst = address.getHostAddress();
            if (!Strings.isNullOrEmpty(rst)) {
                log.info("resolved {} to {} by InetAddress", name, rst);
                return rst;
            }
        } catch (UnknownHostException ignore) {
        }
        log.info("failed to resolve {}", name);
        
        return name;
    }
}
