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

import java.io.Serializable;
import java.util.Objects;

/**
 * AdminResult.
 */
public class AdminResult<T> implements Serializable {
    
    private static final long serialVersionUID = -2792556188993845048L;
    
    private Integer code;
    
    private String message;
    
    private T data;
    
    /**
     * Instantiates a new shenyu result.
     */
    public AdminResult() {
    
    }
    
    /**
     * Instantiates a new shenyu result.
     *
     * @param code    the code
     * @param message the message
     * @param data    the data
     */
    public AdminResult(final Integer code, final String message, final T data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }
    
    
    /**
     * Gets the value of code.
     *
     * @return the value of code
     */
    public Integer getCode() {
        return code;
    }
    
    /**
     * Sets the code.
     *
     * @param code code
     */
    public void setCode(final Integer code) {
        this.code = code;
    }
    
    /**
     * Gets the value of message.
     *
     * @return the value of message
     */
    public String getMessage() {
        return message;
    }
    
    /**
     * Sets the message.
     *
     * @param message message
     */
    public void setMessage(final String message) {
        this.message = message;
    }
    
    /**
     * Gets the value of data.
     *
     * @return the value of data
     */
    public Object getData() {
        return data;
    }
    
    /**
     * Sets the data.
     *
     * @param data data
     */
    public void setData(final T data) {
        this.data = data;
    }
    
    @Override
    public String toString() {
        return "ShenyuAdminResult{"
                + "code=" + code
                + ", message='" + message
                + '\'' + ", data=" + data
                + '}';
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AdminResult)) {
            return false;
        }
        @SuppressWarnings("all")
        AdminResult<T> that = (AdminResult<T>) o;
        return Objects.equals(code, that.code) && Objects.equals(message, that.message) && Objects.equals(data, that.data);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(code, message, data);
    }
}
