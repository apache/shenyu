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

package org.dromara.soul.dashboard.result;

import lombok.Data;

import java.io.Serializable;

/**
 * CommonResponse.
 *
 * @param <T> the type parameter
 * @author xiaoyu
 */
@Data
public class CommonResponse<T> implements Serializable {

    /**
     * The constant serialVersionUID.
     */
    private static final long serialVersionUID = -2792556188993845048L;

    /**
     * error code.
     */
    private int code;

    /**
     * error message.
     */
    private String msg;

    /**
     * success data.
     */
    private T data;

    /**
     * Instantiates a new Common response.
     */
    public CommonResponse() {
        this(CommonCode.SUCCESS.getCode(), "", null);
    }

    /**
     * init CalvinResponse.
     *
     * @param code code
     * @param msg  msg
     * @param data data
     */
    public CommonResponse(int code, String msg, T data) {
        this.code = code;
        this.msg = msg;
        this.data = data;
    }

    /**
     * Success common response.
     *
     * @param <T> the type parameter
     * @return the common response
     */
    public static <T> CommonResponse<T> success() {
        return success("");
    }


    /**
     * Success common response.
     *
     * @param <T> the type parameter
     * @param msg the msg
     * @return the common response
     */
    public static <T> CommonResponse<T> success(String msg) {
        return success(msg, null);
    }

    /**
     * Success common response.
     *
     * @param <T>  the type parameter
     * @param data the data
     * @return the common response
     */
    public static <T> CommonResponse<T> success(T data) {
        return success("", data);
    }

    /**
     * Success common response.
     *
     * @param <T>  the type parameter
     * @param msg  the msg
     * @param data the data
     * @return the common response
     */
    public static <T> CommonResponse<T> success(String msg, T data) {
        return get(CommonCode.SUCCESS.getCode(), msg, data);
    }

    /**
     * Error common response.
     *
     * @param <T> the type parameter
     * @param msg the msg
     * @return the common response
     */
    public static <T> CommonResponse<T> error(String msg) {
        return error(CommonCode.FAILURE.getCode(), msg);
    }


    /**
     * Error common response.
     *
     * @param <T>  the type parameter
     * @param code the code
     * @param msg  the msg
     * @return the common response
     */
    public static <T> CommonResponse<T> error(int code, String msg) {
        return get(code, msg, null);
    }


    /**
     * Get calvin response.
     *
     * @param <T>  the type parameter
     * @param code the code
     * @param msg  the msg
     * @param data the data
     * @return the calvin response
     */
    private static <T> CommonResponse<T> get(int code, String msg, T data) {
        return new CommonResponse<>(code, msg, data);
    }

    /**
     * Is success boolean.
     *
     * @return the boolean
     */
    public boolean isSuccess() {
        return this.code == CommonCode.SUCCESS.getCode();
    }

    @Override
    public String toString() {
        return "CalvinResponse [code=" + code + ", msg=" + msg + ", data=" + data
                + "]";
    }

}
