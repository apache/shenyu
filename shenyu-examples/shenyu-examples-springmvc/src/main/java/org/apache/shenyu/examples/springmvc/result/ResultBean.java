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

package org.apache.shenyu.examples.springmvc.result;

/**
 * ResultBean.
 */
public class ResultBean {

    /**
     * code describe status.
     */
    private Integer code;

    /**
     * msg describe result.
     */
    private String msg;

    /**
     * result data.
     */
    private Object data;

    public ResultBean() {
    }

    public ResultBean(final Integer code, final String msg, final Object data) {
        this.code = code;
        this.msg = msg;
        this.data = data;
    }

    /**
     * Get code.
     *
     * @return code
     */
    public Integer getCode() {
        return code;
    }

    /**
     * Set code.
     *
     * @param code code
     */
    public void setCode(final Integer code) {
        this.code = code;
    }

    /**
     * Get msg.
     *
     * @return msg
     */
    public String getMsg() {
        return msg;
    }

    /**
     * Set msg.
     *
     * @param msg msg
     */
    public void setMsg(final String msg) {
        this.msg = msg;
    }

    /**
     * Get data.
     *
     * @return data
     */
    public Object getData() {
        return data;
    }

    /**
     * Set data.
     *
     * @param data data
     */
    public void setData(final Object data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return "ResultBean{" + "code=" + code + ", msg='" + msg + '\'' + ", data=" + data + '}';
    }

}
