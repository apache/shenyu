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
     * The constant REQUESTDTO.
     */
    String REQUESTDTO = "requestDTO";

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
    String SIGN_PARAMS_ERROR = "认证参数传入不完整!";

    /**
     * The constant SIGN_APP_KEY_IS_NOT_EXIST.
     */
    String SIGN_APP_KEY_IS_NOT_EXIST = "认证签名APP_KEY,不存在";

    /**
     * The constant SIGN_PATH_NOT_EXIST.
     */
    String SIGN_PATH_NOT_EXIST = "认证Key未配置路径获取未匹配";

    /**
     * The constant SIGN_VALUE_IS_ERROR.
     */
    String SIGN_VALUE_IS_ERROR = "签名值错误！";

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


}

