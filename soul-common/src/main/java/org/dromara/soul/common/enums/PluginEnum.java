/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Arrays;

/**
 * PluginEnum.
 *
 * @author xiaoyu(Myth)
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
     * Waf plugin enum.
     */
    WAF(10, 0, "waf"),

    /**
     * Rate limiter plugin enum.
     */
    RATE_LIMITER(20, 0, "rate_limiter"),

    /**
     * Rewrite plugin enum.
     */
    REWRITE(30, 0, "rewrite"),

    /**
     * Redirect plugin enum.
     */
    REDIRECT(40, 0, "redirect"),

    /**
     * Divide plugin enum.
     */
    DIVIDE(50, 0, "divide"),

    /**
     * webSocket plugin enum.
     */
    WEB_SOCKET(51, 0, "webSocket"),

    /**
     * Dubbo plugin enum.
     */
    DUBBO(60, 0, "dubbo"),

    /**
     * springCloud plugin enum.
     */
    SPRING_CLOUD(70, 0, "springCloud"),

    /**
     * Monitor plugin enum.
     */
    MONITOR(80, 0, "monitor");

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
}
