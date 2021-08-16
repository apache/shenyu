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

package org.apache.shenyu.integratedtest.common.dto;

public class AdminResponse<T> {

    private Integer code;

    private String message;

    private T data;

    /**
     * Get the code.
     *
     * @return the code
     */
    public Integer getCode() {
        return code;
    }

    /**
     * Set the code.
     *
     * @param code the code
     */
    public void setCode(final Integer code) {
        this.code = code;
    }

    /**
     * Get the message.
     *
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Set the message.
     *
     * @param message the message
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    /**
     * Get the data.
     *
     * @return the data
     */
    public T getData() {
        return data;
    }

    /**
     * Set the data.
     *
     * @param data the data
     */
    public void setData(final T data) {
        this.data = data;
    }
}
