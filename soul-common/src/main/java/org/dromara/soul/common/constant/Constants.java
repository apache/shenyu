/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.common.constant;

/**
 * Constants.
 *
 * @author xiaoyu(Myth)
 */
public interface Constants {

    String REQUESTDTO = "requestDTO";

    String CLIENT_RESPONSE_ATTR = "webHandlerClientResponse";

    String DUBBO_RPC_RESULT = "dubbo_rpc_result";

    String CLIENT_RESPONSE_RESULT_TYPE = "webHandlerClientResponseResultType";

    String DUBBO_RPC_PARAMS = "dubbo_rpc_params";

    String MODULE = "module";

    String METHOD = "method";

    String CONTENT = "content";

    String APP_KEY = "appKey";

    String EXT_INFO = "extInfo";

    String HTTP_METHOD = "httpMethod";

    String RPC_TYPE = "rpcType";

    String SIGN = "sign";

    String TIMESTAMP = "timestamp";

    int RETRY = 3;

    String SKYWAY_DISRUPTOR_THREAD_NAME = "skyway-disruptor";

    String SKYWAY_THREAD_NAME = "skyway-thread";

    String REJECT_MSG = " You are forbidden to visit";

    String REWRITE_URI = "rewrite_uri";

    String HTTP_ERROR_RESULT = "this is bad request or fuse ing  Please try again later";

    String DUBBO_ERROR_RESULT = "dubbo rpc have error or fuse ing  Please check your param and  try again later";

    String TIMEOUT_RESULT = "this request is time out  Please try again later";

    String UPSTREAM_NOT_FIND = "this can not rule upstream please check you config!";

    String TOO_MANY_REQUESTS = "the request is too fast please try again later";

    String SIGN_IS_NOT_PASS = "sign is not pass,Please check you sign algorithm!";

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


}

