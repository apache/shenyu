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

package org.apache.shenyu.alert.strategy;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * common robot http response entity.
 */
public class CommonRobotNotifyResp {
    
    /**
     * error code.
     */
    @JsonProperty(value = "errcode")
    private Integer errCode;
    
    /**
     * error message.
     */
    @JsonProperty(value = "errmsg")
    private String errMsg;
    
    /**
     * code.
     */
    private Integer code;
    
    /**
     * message.
     */
    private String msg;
    
    public CommonRobotNotifyResp() {
    }
    
    public CommonRobotNotifyResp(final Integer errCode, final String errMsg, final Integer code, final String msg) {
        this.errCode = errCode;
        this.errMsg = errMsg;
        this.code = code;
        this.msg = msg;
    }
    
    /**
     * get error code.
     * @return error code
     */
    public Integer getErrCode() {
        return errCode;
    }
    
    /**
     * set error code.
     * @param errCode error code
     */
    public void setErrCode(final Integer errCode) {
        this.errCode = errCode;
    }
    
    /**
     * get error msg.
     * @return msg
     */
    public String getErrMsg() {
        return errMsg;
    }
    
    /**
     * set error msg.
     * @param errMsg error msg
     */
    public void setErrMsg(final String errMsg) {
        this.errMsg = errMsg;
    }
    
    /**
     * get code.
     * @return code
     */
    public Integer getCode() {
        return code;
    }
    
    /**
     * set code.
     * @param code code
     */
    public void setCode(final Integer code) {
        this.code = code;
    }
    
    /**
     * get msg.
     * @return msg
     */
    public String getMsg() {
        return msg;
    }
    
    /**
     * set msg.
     * @param msg msg
     */
    public void setMsg(final String msg) {
        this.msg = msg;
    }
    
}
