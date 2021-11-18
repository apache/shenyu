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
    
    private ShenyuRequestHeaderToRpcContext headerToRpcContext;

    /**
     * get headerToRpcContext.
     *
     * @return headerToRpcContext
     */
    public ShenyuRequestHeaderToRpcContext getHeaderToRpcContext() {
        return headerToRpcContext;
    }

    /**
     * set headerToRpcContext.
     *
     * @param headerToRpcContext headerToRpcContext
     */
    public void setHeaderToRpcContext(final ShenyuRequestHeaderToRpcContext headerToRpcContext) {
        this.headerToRpcContext = headerToRpcContext;
    }

    public class ShenyuRequestHeaderToRpcContext {

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
         * need to be covered header value and transmit to rpc context.
         * key: headerKey, value: newValue.
         */
        private Map<String, String> coverHeaderToRpcContext;

        /**
         * no args constructor.
         */
        public ShenyuRequestHeaderToRpcContext() {
        }

        /**
         * all args constructor.
         *
         * @param addRpcContext              addRpcContext
         * @param transmitHeaderToRpcContext transmitHeaderToRpcContext
         * @param coverHeaderToRpcContext    coverHeaderToRpcContext
         */
        public ShenyuRequestHeaderToRpcContext(Map<String, String> addRpcContext,
                                               Map<String, String> transmitHeaderToRpcContext,
                                               Map<String, String> coverHeaderToRpcContext) {
            this.addRpcContext = addRpcContext;
            this.transmitHeaderToRpcContext = transmitHeaderToRpcContext;
            this.coverHeaderToRpcContext = coverHeaderToRpcContext;
        }

        public Map<String, String> getAddRpcContext() {
            return addRpcContext;
        }

        public void setAddRpcContext(Map<String, String> addRpcContext) {
            this.addRpcContext = addRpcContext;
        }

        public Map<String, String> getTransmitHeaderToRpcContext() {
            return transmitHeaderToRpcContext;
        }

        public void setTransmitHeaderToRpcContext(Map<String, String> transmitHeaderToRpcContext) {
            this.transmitHeaderToRpcContext = transmitHeaderToRpcContext;
        }

        public Map<String, String> getCoverHeaderToRpcContext() {
            return coverHeaderToRpcContext;
        }

        public void setCoverHeaderToRpcContext(Map<String, String> coverHeaderToRpcContext) {
            this.coverHeaderToRpcContext = coverHeaderToRpcContext;
        }
    }
}
