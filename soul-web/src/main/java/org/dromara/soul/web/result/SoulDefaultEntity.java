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

import lombok.Data;
import org.dromara.soul.common.exception.CommonErrorCode;
import org.springframework.http.HttpStatus;

import java.io.Serializable;

/**
 * SoulWebResult .
 *
 * @author xiaoyu
 */
@Data
public class SoulDefaultEntity implements Serializable {

    private static final long serialVersionUID = -2792556188993845048L;

    private Integer code;

    private String message;

    private Object data;

    /**
     * Instantiates a new Soul result.
     */
    public SoulDefaultEntity() {

    }

    /**
     * Instantiates a new Soul result.
     *
     * @param code    the code
     * @param message the message
     * @param data    the data
     */
    public SoulDefaultEntity(final Integer code, final String message, final Object data) {

        this.code = code;
        this.message = message;
        this.data = data;
    }

    /**
     * return success.
     *
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity success() {
        return success("");
    }

    /**
     * return success.
     *
     * @param msg msg
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity success(final String msg) {
        return success(msg, null);
    }


    /**
     * return success.
     *
     * @param data this is result data.
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity success(final Object data) {
        return success(null, data);
    }

    /**
     * return success.
     *
     * @param msg  this ext msg.
     * @param data this is result data.
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity success(final String msg, final Object data) {
        return get(CommonErrorCode.SUCCESSFUL, msg, data);
    }


    /**
     * Success soul web result.
     *
     * @param code the code
     * @param msg  the msg
     * @param data the data
     * @return the soul web result
     */
    public static SoulDefaultEntity success(final int code, final String msg, final Object data) {
        return get(code, msg, data);
    }

    /**
     * return error .
     *
     * @param msg error msg
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity error(final String msg) {
        return error(CommonErrorCode.ERROR, msg);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity error(final int code, final String msg) {
        return get(code, msg, null);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @param data the data
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity error(final int code, final String msg, final Object data) {
        return get(code, msg, data);
    }


    /**
     * return timeout .
     *
     * @param msg error msg
     * @return {@linkplain SoulDefaultEntity}
     */
    public static SoulDefaultEntity timeout(final String msg) {
        return error(HttpStatus.REQUEST_TIMEOUT.value(), msg);
    }

    private static SoulDefaultEntity get(final int code, final String msg, final Object data) {
        return new SoulDefaultEntity(code, msg, data);
    }

}
