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

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Arrays;
import java.util.List;

/**
 * PluginEnum.
 */
@RequiredArgsConstructor
@Getter
public enum PluginEnum {

    /**
     * Global plugin enum.
     */
    GLOBAL(1, 0, "global"),

    /**
     * Sign plugin enum.
     */
    SIGN(2, 0, "sign"),
    
    /**
     * Jwt plugin enum.
     */
    JWT(9, 0, "jwt"),

    /**
     * OAuth2 plugin enum.
     */
    OAUTH2(3, 0, "oauth2"),

    /**
     * Waf plugin enum.
     */
    WAF(10, 0, "waf"),

    /**
     * Rate limiter plugin enum.
     */
    RATE_LIMITER(20, 0, "rate_limiter"),

    /**
     * Param mapping plugin enum.
     */
    PARAM_MAPPING(22, 0, "param_mapping"),

    /**
     * Context path plugin enum.
     */
    CONTEXT_PATH(25, 0, "context_path"),

    /**
     * Rewrite plugin enum.
     */
    REWRITE(30, 0, "rewrite"),

    /**
     * Redirect plugin enum.
     */
    REDIRECT(40, 0, "redirect"),

    /**
     * Request plugin enum.
     */
    REQUEST(42, 0, "request"),

    /**
     * ModifyResponse plugin enum.
     */
    MODIFY_RESPONSE(44, 0, "modifyResponse"),

    /**
     * Hystrix plugin enum.
     */
    HYSTRIX(45, 0, "hystrix"),

    /**
     * Sentinel plugin enum.
     */
    SENTINEL(45, 0, "sentinel"),

    /**
     * Resilence4J plugin enum.
     */
    RESILIENCE4J(45, 0, "resilience4j"),

    /**
     * Logging plugin enum.
     */
    LOGGING(45, 0, "logging"),

    /**
     * Divide plugin enum.
     */
    DIVIDE(50, 0, "divide"),

    /**
     * springCloud plugin enum.
     */
    SPRING_CLOUD(50, 0, "springCloud"),

    /**
     * webSocket plugin enum.
     */
    WEB_SOCKET(55, 0, "webSocket"),

    /**
     * Param transform plugin enum.
     */
    PARAM_TRANSFORM(58, 0, "paramTransform"),

    /**
     * Dubbo plugin enum.
     */
    DUBBO(60, 0, "dubbo"),

    /**
     * Sofa plugin enum.
     */
    SOFA(60, 0, "sofa"),

    /**
     * Tars plugin enum.
     */
    TARS(60, 0, "tars"),

    /**
     * GPRC plugin enum.
     */
    GRPC(60, 0, "grpc"),

    /**
     * Motan plugin enum.
     */
    MOTAN(60, 0, "motan"),

    /**
     * Monitor plugin enum.
     */
    MONITOR(80, 0, "monitor"),

    /**
     * Response plugin enum.
     */
    RESPONSE(100, 0, "response");


    private final int code;

    private final int role;

    private final String name;

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
