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

package org.apache.shenyu.admin.model.result;

import org.apache.shenyu.common.exception.CommonErrorCode;
import org.springframework.http.HttpStatus;

import java.io.Serializable;

/**
 * ShenyuAdminResult.
 */
public class ShenyuAdminResult extends AdminResult<Object> implements Serializable {

    /**
     * Instantiates a new shenyu result.
     */
    public ShenyuAdminResult() {

    }

    /**
     * Instantiates a new shenyu result.
     *
     * @param code    the code
     * @param message the message
     * @param data    the data
     */
    public ShenyuAdminResult(final Integer code, final String message, final Object data) {
        super(code, message, data);
    }

    /**
     * return success.
     *
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult success() {
        return success("");
    }

    /**
     * return success.
     *
     * @param msg msg
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult success(final String msg) {
        return success(msg, null);
    }

    /**
     * return success.
     *
     * @param data this is result data.
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult success(final Object data) {
        return success(null, data);
    }

    /**
     * return success.
     *
     * @param msg  this ext msg.
     * @param data this is result data.
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult success(final String msg, final Object data) {
        return get(CommonErrorCode.SUCCESSFUL, msg, data);
    }

    /**
     * return error .
     *
     * @param msg error msg
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult error(final String msg) {
        return error(CommonErrorCode.ERROR, msg);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult error(final int code, final String msg) {
        return get(code, msg, null);
    }

    /**
     * return timeout .
     *
     * @param msg error msg
     * @return {@linkplain ShenyuAdminResult}
     */
    public static ShenyuAdminResult timeout(final String msg) {
        return error(HttpStatus.REQUEST_TIMEOUT.value(), msg);
    }

    private static ShenyuAdminResult get(final int code, final String msg, final Object data) {
        return new ShenyuAdminResult(code, msg, data);
    }
    
}
