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

package org.apache.shenyu.e2e.engine.config;

import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.google.common.base.Strings;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest.Parameter;
import org.apache.shenyu.e2e.engine.annotation.ShenYuTest.ServiceConfigure;
import org.apache.shenyu.e2e.engine.annotation.ShenYuValue;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.DockerConfigure.DockerServiceConfigure;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.HostConfigure.HostServiceConfigure;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Getter
public class ShenYuEngineConfigure {
    
    private static final Pattern EXTRACTOR_PATTERN = Pattern.compile("\\{(.*?)\\}");
    
    private static final ObjectMapper mapper = new ObjectMapper();
    
    private Mode mode;
    
    private HostConfigure hostConfigure;
    
    private DockerConfigure dockerConfigure;
    
    public boolean isRunOnHost() {
        return Mode.HOST == mode;
    }
    
    public boolean isRunOnDocker() {
        return Mode.DOCKER == mode;
    }
    
    public enum Mode {
        HOST, DOCKER;
        
        @JsonValue
        public String value() {
            return name();
        }
    }
    
    public enum ServiceType {
        SHENYU_ADMIN,
        SHENYU_GATEWAY,
        EXTERNAL_SERVICE,
    }
    
    @Getter
    @AllArgsConstructor
    public static class HostConfigure {
        private final HostServiceConfigure admin;
        private final HostServiceConfigure gateway;
        private final List<HostServiceConfigure> externalServices;
        
        @Getter
        @AllArgsConstructor
        public static class HostServiceConfigure {
            private final String serviceName;
            private final String baseUrl;
            private final Properties properties;
        }
    }
    
    @Getter
    @AllArgsConstructor
    public static class DockerConfigure {
        private final String dockerComposeFile;
        private final DockerServiceConfigure admin;
        private final DockerServiceConfigure gateway;
        private final List<DockerServiceConfigure> externalServices;
        
        @Getter
        @AllArgsConstructor
        public static class DockerServiceConfigure {
            private final String schema;
            private final String serviceName;
            private final int port;
            private final Properties properties;
        }
    }
    
    public static ShenYuEngineConfigure fromAnnotation(ShenYuTest annotation) {
        final ShenYuTest annotationProxy = newShenYuTestProxy(annotation);
    
        ServiceConfigure[] services = annotationProxy.services();
        ServiceConfigure[] serviceProxies = new ServiceConfigure[services.length];
        for (int i = 0; i< services.length; i++) {
            serviceProxies[i] = newServiceConfigureProxy(i, services[i]);
        }
    
        ShenYuEngineConfigure configure = new ShenYuEngineConfigure();
        configure.mode = annotationProxy.mode();
        if (Mode.DOCKER == annotationProxy.mode()) {
            configure.dockerConfigure = parseDockerServiceConfigures(
                    annotationProxy.dockerComposeFile(),
                    serviceProxies
            );
        } else {
            configure.hostConfigure = parseHostServiceConfigures(serviceProxies);
        }
        return configure;
    }
    
    private static HostConfigure parseHostServiceConfigures(ServiceConfigure[] serviceConfigures) {
        HostServiceConfigure admin = null;
        HostServiceConfigure gateway = null;
        List<HostServiceConfigure> services = new ArrayList<>();
        for (ServiceConfigure configure : serviceConfigures) {
            switch (configure.type()) {
                case SHENYU_ADMIN:
                    admin = parseHostServiceConfigure(configure);
                    break;
                case SHENYU_GATEWAY:
                    gateway = parseHostServiceConfigure(configure);
                    break;
                default:
                    services.add(parseHostServiceConfigure(configure));
            }
        }
        return new HostConfigure(admin, gateway, services);
    }
    
    
    private static HostServiceConfigure parseHostServiceConfigure(ServiceConfigure configure) {
        Preconditions.checkNotNull(configure.serviceName(), "serviceName is non-nullable");
        Preconditions.checkNotNull(configure.baseUrl(), "baseUrl is non-nullable in docker-compose mode");
        
        return new HostServiceConfigure(configure.serviceName(), configure.baseUrl(), toProperties(configure.parameters()));
    }
    
    private static DockerConfigure parseDockerServiceConfigures(String dockerComposeFile, ServiceConfigure[] serviceConfigures) {
        Preconditions.checkNotNull(Strings.emptyToNull(dockerComposeFile), "dockerComposeFile is required in docker-compose mode");
        
        DockerServiceConfigure admin = null;
        DockerServiceConfigure gateway = null;
        List<DockerServiceConfigure> services = new ArrayList<>();
        for (ServiceConfigure configure : serviceConfigures) {
            switch (configure.type()) {
                case SHENYU_ADMIN:
                    admin = parseDockerServiceConfigure(configure);
                    break;
                case SHENYU_GATEWAY:
                    gateway = parseDockerServiceConfigure(configure);
                    break;
                default:
                    services.add(parseDockerServiceConfigure(configure));
            }
        }
        return new DockerConfigure(dockerComposeFile, admin, gateway, services);
    }
    
    private static DockerServiceConfigure parseDockerServiceConfigure(ServiceConfigure configure) {
        Preconditions.checkNotNull(configure.serviceName(), "serviceName is non-nullable");
        
        return new DockerServiceConfigure(configure.schema(), configure.serviceName(), configure.port(), toProperties(configure.parameters()));
    }
    
    private static Properties toProperties(Parameter[] parameters) {
        Properties properties = new Properties();
        Arrays.stream(parameters).forEach(p -> properties.put(p.key(), p.value()));
        return properties;
    }
    
    private static ShenYuTest newShenYuTestProxy(ShenYuTest shenYuTest) {
        return (ShenYuTest) Proxy.newProxyInstance(
                ShenYuTest.class.getClassLoader(),
                new Class[]{ShenYuTest.class},
                (proxy, method, args) -> {
                    if (method.isAnnotationPresent(ShenYuValue.class)) {
                        ShenYuValue annotation = method.getAnnotation(ShenYuValue.class);
                        
                        String propertyKey = annotation.value();
                        propertyKey = propertyKey.substring(1, propertyKey.length() - 1);
                        String property = System.getProperty(propertyKey);
                        if (!Strings.isNullOrEmpty(property)) {
                            return mapper.readValue("\"" + property + "\"", method.getReturnType());
                        }
                    }
                    return normal(method.invoke(shenYuTest), method.getReturnType());
                });
    }
    
    private static ServiceConfigure newServiceConfigureProxy(final int index, ServiceConfigure serviceConfigure) {
        return (ServiceConfigure) Proxy.newProxyInstance(
                ServiceConfigure.class.getClassLoader(),
                new Class[]{ServiceConfigure.class},
                (proxy, method, args) -> {
                    if (method.isAnnotationPresent(ShenYuValue.class)) {
                        ShenYuValue annotation = method.getAnnotation(ShenYuValue.class);
                        
                        String rawKey = annotation.value();
                        String propertyKey = rawKey.replace("[]", "[" + index + "]")
                                .substring(1, rawKey.length() - 1);
                        
                        String property = System.getProperty(propertyKey);
                        if (!Strings.isNullOrEmpty(property)) {
                            return mapper.readValue("\"" + property + "\"", method.getReturnType());
                        }
                    }
                    return normal(method.invoke(serviceConfigure), method.getReturnType());
                });
    }
    
    private static Object normal(Object obj, Class<?> returnType) {
        if (String.class == returnType) {
            String result = obj.toString();
            
            Matcher matcher = EXTRACTOR_PATTERN.matcher(result);
            while (matcher.find()) {
                String pattern = matcher.group(1);
                String key = pattern;
                String val = "null";
                
                if (pattern.contains(":")) {
                    String[] kv = pattern.split(":");
                    key = kv[0];
                    val = kv[1];
                }
                
                result = result.replace(matcher.group(0), System.getProperty(key, val));
            }
            return result;
        }
        return obj;
    }
}
