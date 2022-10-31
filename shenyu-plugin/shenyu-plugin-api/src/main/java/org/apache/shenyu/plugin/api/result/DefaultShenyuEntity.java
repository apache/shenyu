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

import com.fasterxml.jackson.annotation.JsonBackReference;

import java.io.Serializable;

/**
 * DefaultShenyuEntity.
 */
public class DefaultShenyuEntity implements Serializable {

    private static final long serialVersionUID = -2792556188993845048L;
    
    private static final int ERROR = 500;

    private Integer code;

    private String message;

    @JsonBackReference
    private Object data;

    /**
     * Instantiates a new shenyu result.
     *
     * @param code    the code
     * @param message the message
     * @param data    the data
     */
    public DefaultShenyuEntity(final Integer code, final String message, final Object data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    /**
     * Gets code.
     *
     * @return the code
     */
    public Integer getCode() {
        return code;
    }

    /**
     * Sets code.
     *
     * @param code the code
     */
    public void setCode(final Integer code) {
        this.code = code;
    }

    /**
     * Gets message.
     *
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Sets message.
     *
     * @param message the message
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    /**
     * Gets data.
     *
     * @return the data
     */
    public Object getData() {
        return data;
    }

    /**
     * Sets data.
     *
     * @param data the data
     */
    public void setData(final Object data) {
        this.data = data;
    }

    /**
     * return error .
     *
     * @param msg error msg
     * @return {@linkplain DefaultShenyuEntity}
     */
    public static DefaultShenyuEntity error(final String msg) {
        return error(ERROR, msg);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @return {@linkplain DefaultShenyuEntity}
     */
    public static DefaultShenyuEntity error(final int code, final String msg) {
        return get(code, msg, null);
    }

    /**
     * return error .
     *
     * @param code error code
     * @param msg  error msg
     * @param data the data
     * @return {@linkplain DefaultShenyuEntity}
     */
    public static DefaultShenyuEntity error(final int code, final String msg, final Object data) {
        return get(code, msg, data);
    }

    /**
     * return timeout .
     *
     * @param msg error msg
     * @return {@linkplain DefaultShenyuEntity}
     */
    public static DefaultShenyuEntity timeout(final String msg) {
        return error(ERROR, msg);
    }

    private static DefaultShenyuEntity get(final int code, final String msg, final Object data) {
        return new DefaultShenyuEntity(code, msg, data);
    }
}
