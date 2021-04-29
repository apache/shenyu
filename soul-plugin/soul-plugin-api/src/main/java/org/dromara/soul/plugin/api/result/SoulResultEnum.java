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

package org.dromara.soul.plugin.api.result;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * The enum Soul result enum.
 *
 * @author xiaoyu
 */
@Getter
@RequiredArgsConstructor
public enum SoulResultEnum {

    /**
     * Fail soul result enum.
     */
    FAIL(-1, "Internal exception in gateway. Please try again later!"),

    /**
     * Success soul result enum.
     */
    SUCCESS(200, "Access to success!"),

    /**
     * Sign is not pass soul result enum.
     */
    SIGN_IS_NOT_PASS(401, "Sign is not pass soul!"),

    /**
     * Payload too large soul result enum.
     */
    PAYLOAD_TOO_LARGE(403, "Payload too large!"),

    /**
     * Too many requests soul result enum.
     */
    TOO_MANY_REQUESTS(429, "You have been restricted, please try again later!"),

    /**
     * Hystrix plugin fallback, due to a circuit break.
     */
    HYSTRIX_PLUGIN_FALLBACK(429, "HystrixPlugin fallback success, please check your service status!"),

    /**
     * Resilience4J plugin fallback, due to a circuit break.
     */
    RESILIENCE4J_PLUGIN_FALLBACK(429, "Resilience4JPlugin fallback success, please check your service status!"),

    /**
     * Meta data error soul result enum.
     */
    META_DATA_ERROR(430, "Meta data error!"),

    /**
     * Dubbo have body param soul result enum.
     */
    DUBBO_HAVE_BODY_PARAM(431, "Dubbo must have body param, please enter the JSON format in the body!"),

    /**
     * Sofa have body param soul result enum.
     */
    SOFA_HAVE_BODY_PARAM(432, "Sofa must have body param, please enter the JSON format in the body!"),

    /**
     * Tars have body param soul result enum.
     */
    TARS_HAVE_BODY_PARAM(433, "Tars must have body param, please enter the JSON format in the body!"),

    /**
     * Tars invoke soul result enum.
     */
    TARS_INVOKE(434, "Tars invoke error!"),

    /**
     * Grpc have body param soul result enum.
     */
    GRPC_HAVE_BODY_PARAM(435, "Grpc must have body param, please enter the JSON format in the body!"),

    /**
     * Grpc client resultenum.
     */
    GRPC_CLIENT_NULL(436, "Grpc client is null, please check the context path!"),

    /**
     * Motan have body param soul result enum.
     */
    MOTAN_HAVE_BODY_PARAM(437, "Motan must have body param, please enter the JSON format in the body!"),


    /**
     * full selector type enum.
     */
    PARAM_ERROR(-100, "Your parameter error, please check the relevant documentation!"),

    /**
     * Or match mode enum.
     */
    TIME_ERROR(-101, "Your time parameter is incorrect or has expired!"),

    /**
     * Rule not find soul result enum.
     */
    RULE_NOT_FIND(-102, "Rule not found!"),

    /**
     * Service result error soul result enum.
     */
    SERVICE_RESULT_ERROR(-103, "Service invocation exception, or no result is returned!"),

    /**
     * Service timeout soul result enum.
     */
    SERVICE_TIMEOUT(-104, "Service call timeout!"),

    /**
     * Sing time is timeout soul result enum.
     */
    SING_TIME_IS_TIMEOUT(-105, "The signature timestamp has exceeded %s minutes!"),

    /**
     * Cannot find url soul result enum.
     */
    CANNOT_FIND_URL(-106, "Can not find url, please check your configuration!"),

    /**
     * Cannot find selector soul result enum.
     */
    CANNOT_FIND_SELECTOR(-107, "Can not find selector, please check your configuration!"),

    /**
     * The Cannot config springcloud serviceid.
     */
    CANNOT_CONFIG_SPRINGCLOUD_SERVICEID(-108, "You are not configured or do not match springcloud serviceId!"),

    /**
     * The Springcloud serviceid is error.
     */
    SPRINGCLOUD_SERVICEID_IS_ERROR(-109, "SpringCloud serviceId does not exist or is configured incorrectly!"),

    /**
     * The Sentinel block error.
     */
    SENTINEL_BLOCK_ERROR(-110, "The request block by sentinel!"),

    /**
     * The Context path error.
     */
    CONTEXT_PATH_ERROR(-111, "The context path illegal, please check the context path mapping plugin!");

    private final int code;

    private final String msg;
}
