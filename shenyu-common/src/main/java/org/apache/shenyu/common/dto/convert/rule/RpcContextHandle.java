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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Map;

/**
 * this is RequestHandle plugin handle.
 */
public class RpcContextHandle {

    /**
     * rpc context type.
     */
    private String rpcContextType;

    /**
     * rpcContextKey.
     *
     * need to be appended new context value.
     * key: new rpc context key, value: rpc context value.
     */
    private String rpcContextKey;

    /**
     * rpcContextValue.
     * when rpcContextType is addRpcContext, the rpcContextValue is the value of rpcContextKey.
     * when rpcContextType is transmitHeaderToRpcContext, the rpcContextValue is the new key of rpcContext.
     * In this case, if rpcContextValue is blank, default value is same as rpcContextKey.
     */
    private String rpcContextValue;

    /**
     * no args constructor.
     */
    public RpcContextHandle() {
    }

    /**
     * all args constructor.
     *
     * @param rpcContextType  rpcContextType
     * @param rpcContextKey   rpcContextKey
     * @param rpcContextValue rpcContextValue
     */
    public RpcContextHandle(String rpcContextType, String rpcContextKey, String rpcContextValue) {
        this.rpcContextType = rpcContextType;
        this.rpcContextKey = rpcContextKey;
        this.rpcContextValue = rpcContextValue;
    }

    public String getRpcContextType() {
        return rpcContextType;
    }

    public void setRpcContextType(String rpcContextType) {
        this.rpcContextType = rpcContextType;
    }

    public String getRpcContextKey() {
        return rpcContextKey;
    }

    public void setRpcContextKey(String rpcContextKey) {
        this.rpcContextKey = rpcContextKey;
    }

    public String getRpcContextValue() {
        return rpcContextValue;
    }

    public void setRpcContextValue(String rpcContextValue) {
        this.rpcContextValue = rpcContextValue;
    }
}
