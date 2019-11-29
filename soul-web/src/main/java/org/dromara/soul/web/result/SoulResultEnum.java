/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.result;

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
    FAIL(-1, "网关内部异常,请您稍后重试!"),

    /**
     * Success soul result enum.
     */
    SUCCESS(200, "访问成功!"),

    /**
     * Sign is not pass soul result enum.
     */
    SIGN_IS_NOT_PASS(401, "签名未通过!"),

    /**
     * Payload too large soul result enum.
     */
    PAYLOAD_TOO_LARGE(403, "您的文件过大"),

    /**
     * Too many requests soul result enum.
     */
    TOO_MANY_REQUESTS(429, "您已经被限流，请稍后重试!"),

    /**
     * full selector type enum.
     */
    PARAM_ERROR(-100, "您的参数错误,请检查相关文档!"),

    /**
     * Or match mode enum.
     */
    TIME_ERROR(-101, "您的时间参数错误或者已经过期!"),


    /**
     * Rule not find soul result enum.
     */
    RULE_NOT_FIND(-102, "规则未匹配!"),


    /**
     * Service result error soul result enum.
     */
    SERVICE_RESULT_ERROR(-103, "服务调用异常，或者未返回结果"),

    /**
     * Service timeout soul result enum.
     */
    SERVICE_TIMEOUT(-104, "服务调用超时"),

    /**
     * Sing time is timeout soul result enum.
     */
    SING_TIME_IS_TIMEOUT(-105, "签名时间戳已经超过%s分钟!"),

    /**
     * Cannot find url soul result enum.
     */
    CANNOT_FIND_URL(-106, "未能找到合适的调用url,请检查你的配置!"),

    /**
     * Cannot find selector soul result enum.
     */
    CANNOT_FIND_SELECTOR(-107, "未能匹配选择器,请检查你的选择器配置！"),

    /**
     * The Cannot config springcloud serviceid.
     */
    CANNOT_CONFIG_SPRINGCLOUD_SERVICEID(-108, "您并未配置或未匹配springcloud serviceId"),

    /**
     * The Springcloud serviceid is error.
     */
    SPRINGCLOUD_SERVICEID_IS_ERROR(-109, "springCloud serviceId 不存在或者配置错误！或者注册中心配置错误! ");

    private final int code;

    private final String msg;

}
