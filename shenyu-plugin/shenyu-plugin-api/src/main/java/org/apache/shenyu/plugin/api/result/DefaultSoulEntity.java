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

package org.apache.shenyu.plugin.api.result;

import lombok.Data;

import java.io.Serializable;

/**
 * DefaultSoulEntity.
 *
 * @author xiaoyu
 */
@Data
public class DefaultSoulEntity implements Serializable {

    private static final long serialVersionUID = -2792556188993845048L;
    
    private static final int ERROR = 500;
    
    private static final int SUCCESSFUL = 200;

    private Integer code;

    private String message;

    private Object data;

    /**
     * Instantiates a new Soul result.
     *
     * @param code    the code
     * @param message the message
     * @param data    the data
     */
    public DefaultSoulEntity(final Integer code, final String message, final Object data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    /**
     * return success.
     *
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity success() {
        return success("");
    }

    /**
     * return success.
     *
     * @param msg msg
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity success(final String msg) {
        return success(msg, null);
    }

    /**
     * return success.
     *
     * @param data this is result data.
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity success(final Object data) {
        return success(null, data);
    }

    /**
     * return success.
     *
     * @param msg  this ext msg.
     * @param data this is result data.
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity success(final String msg, final Object data) {
        return get(SUCCESSFUL, msg, data);
    }

    /**
     * Success soul web result.
     *
     * @param code the code
     * @param msg  the msg
     * @param data the data
     * @return the soul web result
     */
    public static DefaultSoulEntity success(final int code, final String msg, final Object data) {
        return get(code, msg, data);
    }

    /**
     * return error .
     *
     * @param msg error msg
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity error(final String msg) {
        return error(ERROR, msg);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity error(final int code, final String msg) {
        return get(code, msg, null);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @param data the data
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity error(final int code, final String msg, final Object data) {
        return get(code, msg, data);
    }

    /**
     * return timeout .
     *
     * @param msg error msg
     * @return {@linkplain DefaultSoulEntity}
     */
    public static DefaultSoulEntity timeout(final String msg) {
        return error(ERROR, msg);
    }

    private static DefaultSoulEntity get(final int code, final String msg, final Object data) {
        return new DefaultSoulEntity(code, msg, data);
    }
}
