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
     * RequestId plugin enum.
     */
    REQUEST_ID(20, 0, "request-id"),

    /**
     * Sign plugin enum.
     */
    SIGN(30, 0, "sign"),

    /**
     * Jwt plugin enum.
     */
    JWT(40, 0, "jwt"),

    /**
     * OAuth2 plugin enum.
     */
    OAUTH2(50, 0, "oauth2"),

    /**
     * Waf plugin enum.
     */
    WAF(60, 0, "waf"),

    /**
     * Rate limiter plugin enum.
     */
    RATE_LIMITER(70, 0, "rate_limiter"),

    /**
     * Param mapping plugin enum.
     */
    PARAM_MAPPING(80, 0, "param_mapping"),

    /**
     * Context path plugin enum.
     */
    CONTEXT_PATH(90, 0, "context_path"),

    /**
     * Rewrite plugin enum.
     */
    REWRITE(100, 0, "rewrite"),

    /**
     * Cryptor request plugin enum.
     */
    CRYPTOR_REQUEST(110, 0, "cryptor_request"),

    /**
     * Redirect plugin enum.
     */
    REDIRECT(120, 0, "redirect"),

    /**
     * Request plugin enum.
     */
    REQUEST(130, 0, "request"),

    /**
     * ModifyResponse plugin enum.
     */
    MODIFY_RESPONSE(140, 0, "modifyResponse"),

    /**
     * Hystrix plugin enum.
     */
    HYSTRIX(150, 0, "hystrix"),

    /**
     * Sentinel plugin enum.
     */
    SENTINEL(160, 0, "sentinel"),

    /**
     * Resilence4J plugin enum.
     */
    RESILIENCE4J(170, 0, "resilience4j"),

    /**
     * Logging plugin enum.
     */
    LOGGING(180, 0, "logging"),

    /**
     * Divide plugin enum.
     */
    DIVIDE(190, 0, "divide"),

    /**
     * springCloud plugin enum.
     */
    SPRING_CLOUD(200, 0, "springCloud"),

    /**
     * webSocket plugin enum.
     */
    WEB_SOCKET(210, 0, "websocket"),

    /**
     * Param transform plugin enum.
     */
    PARAM_TRANSFORM(220, 0, "paramTransform"),

    /**
     * Dubbo plugin enum.
     */
    DUBBO(230, 0, "dubbo"),

    /**
     * Sofa plugin enum.
     */
    SOFA(240, 0, "sofa"),

    /**
     * Tars plugin enum.
     */
    TARS(250, 0, "tars"),

    /**
     * GPRC plugin enum.
     */
    GRPC(260, 0, "grpc"),

    /**
     * Motan plugin enum.
     */
    MOTAN(270, 0, "motan"),

    /**
     * Monitor plugin enum.
     */
    MONITOR(280, 0, "monitor"),

    /**
     * Cryptor response plugin enum.
     */
    CRYPTOR_RESPONSE(290, 0, "cryptor_response"),

    /**
     * Response plugin enum.
     */
    RESPONSE(300, 0, "response");

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
     * @return code
     */
    public int getCode() {
        return code;
    }

    /**
     * get role.
     *
     * @return role
     */
    public int getRole() {
        return role;
    }

    /**
     * get name.
     *
     * @return name
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
        return Arrays.asList(DIVIDE.name, GRPC.name, TARS.name);
    }
}
