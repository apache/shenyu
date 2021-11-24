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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * this is RequestHandle plugin handle.
 */
public class RpcContextHandle {

    /**
     * need to be appended new context value.
     * key: new rpc context key, value: rpc context value.
     */
    private Map<String, String> addRpcContext;

    /**
     * transmit http request header to rpc context.
     * key: headerKey, value: rpc context value.
     */
    private Map<String, String> transmitHeaderToRpcContext;

    /**
     * no args constructor.
     */
    public RpcContextHandle() {
    }

    /**
     * all args constructor.
     *
     * @param addRpcContext              addRpcContext
     * @param transmitHeaderToRpcContext transmitHeaderToRpcContext
     */
    public RpcContextHandle(final Map<String, String> addRpcContext, final Map<String, String> transmitHeaderToRpcContext) {
        this.addRpcContext = addRpcContext;
        this.transmitHeaderToRpcContext = transmitHeaderToRpcContext;
    }

    public Map<String, String> getAddRpcContext() {
        return addRpcContext;
    }

    public void setAddRpcContext(final Map<String, String> addRpcContext) {
        this.addRpcContext = addRpcContext;
    }

    public Map<String, String> getTransmitHeaderToRpcContext() {
        return transmitHeaderToRpcContext;
    }

    public void setTransmitHeaderToRpcContext(final Map<String, String> transmitHeaderToRpcContext) {
        this.transmitHeaderToRpcContext = transmitHeaderToRpcContext;
    }
}
