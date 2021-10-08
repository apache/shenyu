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

package org.apache.shenyu.common.enums;

import java.util.Arrays;
import java.util.List;

/**
 * PluginEnum.
 */
public enum PluginEnum {
    
    /**
     * Global plugin enum.
     */
    GLOBAL(10, 0, "global"),
    
    /**
     * Sign plugin enum.
     */
    SIGN(20, 0, "sign"),
    
    /**
     * Jwt plugin enum.
     */
    JWT(30, 0, "jwt"),
    
    /**
     * OAuth2 plugin enum.
     */
    OAUTH2(40, 0, "oauth2"),
    
    /**
     * Waf plugin enum.
     */
    WAF(50, 0, "waf"),
    
    /**
     * Rate limiter plugin enum.
     */
    RATE_LIMITER(60, 0, "rate_limiter"),
    
    /**
     * Param mapping plugin enum.
     */
    PARAM_MAPPING(70, 0, "param_mapping"),
    
    /**
     * Context path plugin enum.
     */
    CONTEXT_PATH(80, 0, "context_path"),
    
    /**
     * Rewrite plugin enum.
     */
    REWRITE(90, 0, "rewrite"),
    
    /**
     * Cryptor request plugin enum.
     */
    CRYPTOR_REQUEST(100, 0, "cryptor_request"),
    
    /**
     * Redirect plugin enum.
     */
    REDIRECT(110, 0, "redirect"),
    
    /**
     * Request plugin enum.
     */
    REQUEST(120, 0, "request"),
    
    /**
     * Hystrix plugin enum.
     */
    HYSTRIX(130, 0, "hystrix"),
    
    /**
     * Sentinel plugin enum.
     */
    SENTINEL(140, 0, "sentinel"),
    
    /**
     * Resilence4J plugin enum.
     */
    RESILIENCE4J(150, 0, "resilience4j"),
    
    /**
     * Logging plugin enum.
     */
    LOGGING(160, 0, "logging"),
    
    /**
     * Monitor plugin enum.
     */
    MONITOR(170, 0, "monitor"),
    
    /**
     * Divide plugin enum.
     */
    DIVIDE(200, 0, "divide"),
    
    /**
     * springCloud plugin enum.
     */
    SPRING_CLOUD(200, 0, "springCloud"),
    
    /**
     * webSocket plugin enum.
     */
    WEB_SOCKET(200, 0, "websocket"),
    
    /**
     * Web client plugin enum.
     */
    WEB_CLIENT(210, 0, "webClient"),
    
    /**
     * Netty http client plugin enum.
     */
    NETTY_HTTP_CLIENT(210, 0, "nettyHttpClient"),

    /**
     * ModifyResponse plugin enum.
     */
    MODIFY_RESPONSE(220, 0, "modifyResponse"),
    
    /**
     * Param transform plugin enum.
     */
    RPC_PARAM_TRANSFORM(300, 0, "paramTransform"),
    
    /**
     * Dubbo plugin enum.
     */
    DUBBO(310, 0, "dubbo"),
    
    /**
     * Sofa plugin enum.
     */
    SOFA(310, 0, "sofa"),
    
    /**
     * Tars plugin enum.
     */
    TARS(310, 0, "tars"),
    
    /**
     * GPRC plugin enum.
     */
    GRPC(310, 0, "grpc"),
    
    /**
     * Motan plugin enum.
     */
    MOTAN(310, 0, "motan"),
    
    /**
     * Cryptor response plugin enum.
     */
    CRYPTOR_RESPONSE(410, 0, "cryptor_response"),
    
    /**
     * Response plugin enum.
     */
    RESPONSE(420, 0, "response");

    private final int code;

    private final int role;

    private final String name;

    /**
     * all args constructor.
     *
     * @param code code
     * @param role role
     * @param name name
     */
    PluginEnum(final int code, final int role, final String name) {
        this.code = code;
        this.role = role;
        this.name = name;
    }
    
    /**
     * get code.
     *
     * @return code code
     */
    public int getCode() {
        return code;
    }
    
    /**
     * get role.
     *
     * @return role role
     */
    public int getRole() {
        return role;
    }
    
    /**
     * get name.
     *
     * @return name name
     */
    public String getName() {
        return name;
    }
    
    /**
     * get plugin enum by name.
     *
     * @param name plugin name.
     * @return plugin enum.
     */
    public static PluginEnum getPluginEnumByName(final String name) {
        return Arrays.stream(PluginEnum.values())
                .filter(pluginEnum -> pluginEnum.getName().equals(name))
                .findFirst().orElse(PluginEnum.GLOBAL);
    }
    
    /**
     * get upstream plugin names.
     *
     * @return List string
     */
    public static List<String> getUpstreamNames() {
        return Arrays.asList(DIVIDE.name, GRPC.name, TARS.name, SPRING_CLOUD.name);
    }
}
