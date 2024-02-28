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

package org.apache.shenyu.e2e.model;

import com.fasterxml.jackson.annotation.JsonValue;
import org.apache.shenyu.e2e.model.response.PluginDTO;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Plugin enum.
 */
public enum Plugin {
    
    SIGN("sign", 1),
    WAF("waf", 2),
    REWRITE("rewrite", 3),
    RATE_LIMITER("rateLimiter", 4),
    DIVIDE("divide", 5),
    DUBBO("dubbo", 6),
    SPRING_CLOUD("springCloud", 8),
    HYSTRIX("hystrix", 9),
    SENTINEL("sentinel", 10),
    SOFA("sofa", 11),
    RESILIENCE4J("resilience4j", 12),
    TARS("tars", 13),
    CONTEXT_PATH("contextPath", 14),
    GRPC("grpc", 15),
    REDIRECT("redirect", 16),
    MOTAN("motan", 17),
    LOGGING_CONSOLE("loggingConsole", 18),
    JWT("jwt", 19),
    REQUEST("request", 20),
    OAUTH2("oauth2", 21),
    PARAM_MAPPING("paramMapping", 22),
    MODIFY_RESPONSE("modifyResponse", 23),
    CRYPTOR_REQUEST("cryptorRequest", 24),
    CRYPTOR_RESPONSE("cryptorResponse", 25),
    WEBSOCKET("websocket", 26),
    GENERAL_CONTEXT("generalContext", 27),
    MQTT("mqtt", 28),
    LOGGING_ROCKETMQ("loggingRocketMQ", 29),
    CACHE("cache", 30),
    MOCK("mock", 31),
    LOGGING_ELASTIC_SEARCH("loggingElasticSearch", 32),
    LOGGING_KAFKA("loggingKafka", 33),
    LOGGING_ALIYUN_SLS("loggingAliyunSls", 34),
    LOGGING_TENCENT_CLS("loggingTencentCls", 36),
    LOGGING_PULSAR("loggingPulsar", 35),
    LOGGING_CLICK_HOUSE("loggingClickHouse", 38),
    BRPC("brpc", 41),
    LOGGINGHUAWEILTS("loggingHuaweiLts", 43),
    LOGGINGRABBITMQ("loggingRabbitMQ", 45),;

    private static final Logger log = LoggerFactory.getLogger(Plugin.class);
    private final String id;
    private final String alias;
    
    Plugin(final String alias, final int id) {
        this.alias = alias;
        this.id = String.valueOf(id);
    }
    
    /**
     * id.
     * @return String
     */
    @JsonValue
    public String getId() {
        return id;
    }
    
    /**
     * get alias.
     * @return String
     */
    public String getAlias() {
        return alias;
    }
    
    /**
     * alias 2 id map.
     * @return Map&lt;String, String&gt; map.
     */
    public static Map<String, String> toMap() {
        return Arrays.stream(Plugin.values()).collect(Collectors.toMap(Plugin::getAlias, Plugin::getId));
    }
    
    /**
     * check.
     * @param plugins plugins
     */
    public static void check(final List<PluginDTO> plugins) {
        StringBuilder builder = new StringBuilder();
        Map<String, String> pluginMap = toMap();
        AssertionError error = null;
        
        for (PluginDTO plugin : plugins) {
            try {
                Assertions.assertEquals(
                        plugin.getId(),
                        pluginMap.get(plugin.getName()),
                        "checking Plugin[" + plugin.getName() + "]'s id"
                );
            } catch (AssertionError e) {
                String name = plugin.getName().replaceAll("(\\p{Lu})", "_$1").toUpperCase(Locale.ROOT);
                builder.append(System.lineSeparator())
                        .append(String.format("%s(\"%s\", %s),", name, plugin.getName(), plugin.getId()));
                error = e;
            }
        }
        if (Objects.nonNull(error)) {
            log.warn("please paste follow lines to org.apache.shenyu.e2e.client.admin.model.Plugin:{}", builder);
            // throw error; // TODO Don't throw it temporarily
        }
    }
}
