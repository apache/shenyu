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

package org.dromara.soul.common.constant;

/**
 * Constants.
 *
 * @author xiaoyu(Myth)
 */
public interface Constants {

    /**
     * The constant APP_PARAM.
     */
    String APP_PARAM = "appParam";

    /**
     * The constant context.
     */
    String CONTEXT = "context";

    /**
     * The constant META_DATA.
     */
    String META_DATA = "metaData";

    /**
     * The constant CLIENT_RESPONSE_ATTR.
     */
    String CLIENT_RESPONSE_ATTR = "webHandlerClientResponse";

    /**
     * The constant DUBBO_RPC_RESULT.
     */
    String DUBBO_RPC_RESULT = "dubbo_rpc_result";

    /**
     * The constant DUBBO_RPC_RESULT_EMPTY.
     */
    String DUBBO_RPC_RESULT_EMPTY = "dubbo has not return value!";

    /**
     * The constant DUBBO_TAG_ROUTE.
     */
    String DUBBO_TAG_ROUTE = "Dubbo_Tag_Route";

    /**
     * The constant SOFA_RPC_RESULT.
     */
    String SOFA_RPC_RESULT = "sofa_rpc_result";

    /**
     * The constant SOFA_RPC_RESULT_EMPTY.
     */
    String SOFA_RPC_RESULT_EMPTY = "sofa has not return value!";

    /**
     * The constant CLIENT_RESPONSE_RESULT_TYPE.
     */
    String CLIENT_RESPONSE_RESULT_TYPE = "webHandlerClientResponseResultType";

    /**
     * The constant CLIENT_RESPONSE_CONN_ATTR.
     */
    String CLIENT_RESPONSE_CONN_ATTR = "nettyClientResponseConnection";

    /**
     * The constant HTTP_TIME_OUT.
     */
    String HTTP_TIME_OUT = "httpTimeOut";

    /**
     * The constant HTTP_RETRY.
     */
    String HTTP_RETRY = "httpRetry";

    /**
     * Original response Content-Type attribute name.
     */
    String ORIGINAL_RESPONSE_CONTENT_TYPE_ATTR = "original_response_content_type";

    /**
     * The constant HTTP_URL.
     */
    String HTTP_URL = "httpUrl";

    /**
     * The constant DUBBO_PARAMS.
     */
    String DUBBO_PARAMS = "dubbo_params";

    /**
     * The constant SOFA_PARAMS.
     */
    String SOFA_PARAMS = "sofa_params";

    /**
     * The constant DECODE.
     */
    String DECODE = "UTF-8";

    /**
     * The constant MODULE.
     */
    String MODULE = "module";

    /**
     * The constant METHOD.
     */
    String METHOD = "method";

    /**
     * The constant APP_KEY.
     */
    String APP_KEY = "appKey";

    /**
     * The constant RPC_TYPE.
     */
    String RPC_TYPE = "rpcType";

    /**
     * The constant SIGN.
     */
    String SIGN = "sign";

    /**
     * The constant PATH.
     */
    String PATH = "path";

    /**
     * The constant VERSION.
     */
    String VERSION = "version";

    /**
     * The constant SIGN_PARAMS_ERROR.
     */
    String SIGN_PARAMS_ERROR = "sign parameters are incomplete!";

    /**
     * The constant SIGN_APP_KEY_IS_NOT_EXIST.
     */
    String SIGN_APP_KEY_IS_NOT_EXIST = "sign appKey does not exist.";

    /**
     * The constant SIGN_PATH_NOT_EXIST.
     */
    String SIGN_PATH_NOT_EXIST = "you have not configured the sign path.";

    /**
     * The constant SIGN_VALUE_IS_ERROR.
     */
    String SIGN_VALUE_IS_ERROR = "signature value is error!";

    /**
     * The constant TIMESTAMP.
     */
    String TIMESTAMP = "timestamp";

    /**
     * The constant REJECT_MSG.
     */
    String REJECT_MSG = " You are forbidden to visit";

    /**
     * The constant REWRITE_URI.
     */
    String REWRITE_URI = "rewrite_uri";

    /**
     * The constant LINE_SEPARATOR.
     */
    String LINE_SEPARATOR = System.getProperty("line.separator");

    /**
     * hystrix withExecutionIsolationSemaphoreMaxConcurrentRequests.
     */
    int MAX_CONCURRENT_REQUESTS = 100;

    /**
     * hystrix  withCircuitBreakerErrorThresholdPercentage.
     */
    int ERROR_THRESHOLD_PERCENTAGE = 50;

    /**
     * hystrix withCircuitBreakerRequestVolumeThreshold.
     */
    int REQUEST_VOLUME_THRESHOLD = 20;

    /**
     * hystrix withCircuitBreakerSleepWindowInMilliseconds.
     */
    int SLEEP_WINDOW_INMILLISECONDS = 5000;

    /**
     * The constant TIME_OUT.
     */
    long TIME_OUT = 3000;

    /**
     * The constant COLONS.
     */
    String COLONS = ":";

    /**
     * hystrix thead pool core size.
     */
    int HYSTRIX_THREAD_POOL_CORE_SIZE = 10;

    /**
     * hystrix thread pool max size.
     */
    int HYSTRIX_THREAD_POOL_MAX_SIZE = 10;

    /**
     * hystrix thread pool keep alive time minutes.
     */
    int HYSTRIX_THREAD_KEEP_ALIVE_TIME_MINUTE = 1;

    /**
     * hystrix thread pool queue size.
     */
    int HYSTRIX_THREAD_POOL_QUEUE_SIZE = 12;


    /**
     * ratelimiter timeoutDurationRate.
     */
    int TIMEOUT_DURATION_RATE = 5000;

    /**
     * ratelimiter limitRefreshPeriod.
     */
    int LIMIT_REFRESH_PERIOD = 500;

    /**
     * ratelimiter limitForPeriod.
     */
    int LIMIT_FOR_PERIOD = 50;

    /**
     * circuitBreaker circuitEnable.
     */
    int CIRCUIT_ENABLE = 0;

    /**
     * circuitBreaker timeoutDuration.
     */
    long TIMEOUT_DURATION = 30000;

    /**
     * circuitBreaker slidingWindowSize.
     */
    int SLIDING_WINDOW_SIZE = 100;

    /**
     * circuitBreaker slidingWindowType.
     */
    int SLIDING_WINDOW_TYPE = 0;

    /**
     * circuitBreaker minimumNumberOfCalls.
     */
    int MINIMUM_NUMBER_OF_CALLS = 100;

    /**
     * circuitBreaker waitIntervalFunctionInOpenState.
     */
    int WAIT_INTERVAL_FUNCTION_IN_OPEN_STATE = 60000;

    /**
     * circuitBreaker waitIntervalFunctionInOpenState.
     */
    int PERMITTED_NUMBER_OF_CALLS_IN_HALF_OPEN_STATE = 10;

    /**
     * circuitBreaker failureRateThreshold.
     */
    float FAILURE_RATE_THRESHOLD = 50;

    /**
     * circuitBreaker automaticTransitionFromOpenToHalfOpenEnabled.
     */
    boolean AUTOMATIC_TRANSITION_FROM_OPEN_TO_HALF_OPEN_ENABLED = false;

    /**
     * Enable the flow rule.
     */
    int SENTINEL_ENABLE_FLOW_RULE = 1;

    /**
     * Sentinel qps flow grade.
     */
    int SENTINEL_QPS_FLOW_GRADE = 1;

    /**
     * Sentinel flow reject behavior.
     */
    int SENTINEL_FLOW_REJECT = 0;

    /**
     * Enable the degrade rule.
     */
    int SENTINEL_ENABLE_DEGRADE_RULE = 1;

    /**
     * Sentinel response RT degrade rule.
     */
    int SENTINEL_RESPONSE_RULE_GRADE = 0;
}

