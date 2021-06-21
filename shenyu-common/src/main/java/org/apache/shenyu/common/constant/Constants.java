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

package org.apache.shenyu.common.constant;

/**
 * Constants.
 */
public interface Constants {

    /**
     * The constant path template.
     */
    char LEFT = '{';

    /**
     * The constant path template.
     */
    char RIGHT = '}';

    /**
     * The constant path template.
     */
    String PATTERN = "([^/]*)";

    /**
     * The constant APP_PARAM.
     */
    String APP_PARAM = "appParam";

    /**
     * The constant context.
     */
    String CONTEXT = "context";

    /**
     * The constant context path.
     */
    String CONTEXT_PATH = "contextPath";

    /**
     * The constant META_DATA.
     */
    String META_DATA = "metaData";

    /**
     * The constant HOST.
     */
    String HOST = "host";

    /**
     * The constant PORT.
     */
    String PORT = "port";

    /**
     * The constant rule name.
     */
    String RULE_NAME = "ruleName";

    /**
     * The constant service name.
     */
    String SERVICE_NAME = "serviceName";

    /**
     * The constant method name.
     */
    String METHOD_NAME = "methodName";

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
     * The constant DUBBO_REFRENCE_INVOKER.
     */
    String DUBBO_REFRENCE_INVOKER = "invoker";

    /**
     * The constant DUBBO_PROVIDER_VERSION.
     */
    String DUBBO_PROVIDER_VERSION = "release";

    /**
     * The constant DUBBO_PROVIDER_VERSION.
     */
    int DUBBO_SUPPORT_ASYNC_VERSION = 2070300;

    /**
     * The constant SOFA_RPC_RESULT.
     */
    String SOFA_RPC_RESULT = "sofa_rpc_result";

    /**
     * The constant SOFA_RPC_RESULT_EMPTY.
     */
    String SOFA_RPC_RESULT_EMPTY = "sofa has not return value!";

    /**
     * The constant TARS_RPC_RESULT.
     */
    String TARS_RPC_RESULT = "tars_rpc_result";

    /**
     * The constant GRPC_RPC_RESULT.
     */
    String GRPC_RPC_RESULT = "grpc_rpc_result";

    /**
     * The constant RPC_RESULT.
     */
    String RPC_RESULT = "rpc_result";

    /**
     * The constant MOTAN_RPC_RESULT.
     */
    String MOTAN_RPC_RESULT = "motan_rpc_result";

    /**
     * The constant TARS_RPC_RESULT_EMPTY.
     */
    String TARS_RPC_RESULT_EMPTY = "tars has not return value!";

    /**
     * The constant MOTAN_RPC_RESULT_EMPTY.
     */
    String MOTAN_RPC_RESULT_EMPTY = "motan has not return value!";

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
     * The constant PARAM_TRANSFORM.
     */
    String PARAM_TRANSFORM = "param_transform";

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
     * The constant EXT_INFO.
     */
    String EXT_INFO = "extInfo";

    /**
     * The constant PATH_VARIABLE.
     */
    String PATH_VARIABLE = "pathVariable";

    /**
     * The constant HTTP_METHOD.
     */
    String HTTP_METHOD = "httpMethod";

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
     * The constant HTTP_ERROR_RESULT.
     */
    String HTTP_ERROR_RESULT = "this is bad request or fuse ing please try again later";

    /**
     * The constant DUBBO_ERROR_RESULT.
     */
    String DUBBO_ERROR_RESULT = "dubbo rpc have error or fuse ing please check your param and  try again later";

    /**
     * The constant SPRING_CLOUD_ERROR_RESULT.
     */
    String SPRING_CLOUD_ERROR_RESULT = "spring cloud rpc have error or fuse ing please check your param and  try again later";

    /**
     * The constant TIMEOUT_RESULT.
     */
    String TIMEOUT_RESULT = "this request is time out  Please try again later";

    /**
     * The constant UPSTREAM_NOT_FIND.
     */
    String UPSTREAM_NOT_FIND = "this can not rule upstream please check you configuration!";

    /**
     * The constant TOO_MANY_REQUESTS.
     */
    String TOO_MANY_REQUESTS = "the request is too fast please try again later";

    /**
     * The constant SIGN_IS_NOT_PASS.
     */
    String SIGN_IS_NOT_PASS = "sign is not pass,Please check you sign algorithm!";

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
     * The constant HYPHEN.
     */
    String HYPHEN = "-";

    /**
     * The constant SlASH.
     */
    String SLASH = "/";

    /**
     * The constant Period.
     */
    String PERIOD = ".";

    String REQUEST_LATENCY = "REQUEST_LATENCY";

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

    /**
     * Sentinel degrade rule default min request.
     */
    int SENTINEL_MIN_REQUEST_AMOUNT = 5;

    /**
     * Sentinel degrade rule default slow ratio threshold.
     */
    double SENTINEL_SLOW_RATIO_THRESHOLD = 1.0d;

    /**
     * Sentinel degrade rule default stat intervals.
     */
    int SENTINEL_STAT_INTERVALS = 1;

    /**
     * default warmup.
     */
    int DEFAULT_WARMUP = 10 * 60 * 1000;

    /**
     * default register type.
     */
    String DEFAULT_REGISTER_TYPE = "http";

    /**
     * is checked.
     */
    String IS_CHECKED = "checked";

    /**
     * default checked value.
     */
    String DEFAULT_CHECK_VALUE = "true";

    /**
     * zombie check times.
     */
    String ZOMBIE_CHECK_TIMES = "zombieCheckTimes";

    /**
     * default zombie check times value.
     */
    String ZOMBIE_CHECK_TIMES_VALUE = "5";

    /**
     * scheduled time.
     */
    String SCHEDULED_TIME = "scheduledTime";

    /**
     * default scheduled time value.
     */
    String SCHEDULED_TIME_VALUE = "10";

    /**
     * default headerMaxSize value.
     */
    int HEADER_MAX_SIZE = 10240;

    /**
     * default requestMaxSize value.
     */
    int REQUEST_MAX_SIZE = 102400;

    /**
     * String default.
     */
    String DEFAULT = "DEFAULT";

    /**
     * String q.
     */
    default void findConstants() {
    }
}

