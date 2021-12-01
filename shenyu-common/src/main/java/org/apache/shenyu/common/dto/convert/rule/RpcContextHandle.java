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

import java.util.List;
import java.util.Objects;

/**
 * this is RequestHandle plugin handle.
 */
public class RpcContextHandle {

    /**
     * rpcType.
     */
    private String rpcType;

    /**
     * RpcContextHandleContent.
     */
    private List<RpcContextHandleContent> rpcContextHandleContents;

    /**
     * no args constructor.
     */
    public RpcContextHandle() {
    }

    /**
     * all args constructor.
     *
     * @param rpcType                  rpc
     * @param rpcContextHandleContents rpcContextHandleContents
     */
    public RpcContextHandle(final String rpcType, final List<RpcContextHandleContent> rpcContextHandleContents) {
        this.rpcType = rpcType;
        this.rpcContextHandleContents = rpcContextHandleContents;
    }

    /**
     * get rpcType.
     *
     * @return rpcType
     */
    public String getRpcType() {
        return rpcType;
    }

    /**
     * set rpcType.
     *
     * @param rpcType rpcType
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }

    /**
     * get rpcContextHandleContents.
     *
     * @return rpcContextHandleContents
     */
    public List<RpcContextHandleContent> getRpcContextHandleContents() {
        return rpcContextHandleContents;
    }

    /**
     * set rpcContextHandleContents.
     *
     * @param rpcContextHandleContents rpcContextHandleContents
     */
    public void setRpcContextHandleContents(final List<RpcContextHandleContent> rpcContextHandleContents) {
        this.rpcContextHandleContents = rpcContextHandleContents;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        RpcContextHandle that = (RpcContextHandle) o;
        return Objects.equals(rpcType, that.rpcType) && Objects.equals(rpcContextHandleContents, that.rpcContextHandleContents);
    }

    @Override
    public int hashCode() {
        return Objects.hash(rpcType, rpcContextHandleContents);
    }

    @Override
    public String toString() {
        return "RpcContextHandle{"
                + "rpcType='" + rpcType + '\''
                + ", rpcContextHandleContents=" + rpcContextHandleContents
                + '}';
    }

    public class RpcContextHandleContent {
        /**
         * rpc context type.
         */
        private String rpcContextType;

        /**
         * rpcContextKey.
         * <p>
         * need to be added new context value.
         * key: new rpc context key, value: rpc context value.
         * </p>
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
        public RpcContextHandleContent() {
        }

        /**
         * all args constructor.
         *
         * @param rpcContextType  rpcContextType
         * @param rpcContextKey   rpcContextKey
         * @param rpcContextValue rpcContextValue
         */
        public RpcContextHandleContent(final String rpcContextType, final String rpcContextKey, final String rpcContextValue) {
            this.rpcContextType = rpcContextType;
            this.rpcContextKey = rpcContextKey;
            this.rpcContextValue = rpcContextValue;
        }

        /**
         * get rpcContextType.
         *
         * @return rpcContextType
         */
        public String getRpcContextType() {
            return rpcContextType;
        }

        /**
         * set rpcContextType.
         *
         * @param rpcContextType rpcContextType
         */
        public void setRpcContextType(final String rpcContextType) {
            this.rpcContextType = rpcContextType;
        }

        /**
         * get rpcContextKey.
         *
         * @return rpcContextKey
         */
        public String getRpcContextKey() {
            return rpcContextKey;
        }

        /**
         * set rpcContextKey.
         *
         * @param rpcContextKey rpcContextKey
         */
        public void setRpcContextKey(final String rpcContextKey) {
            this.rpcContextKey = rpcContextKey;
        }

        /**
         * get rpcContextValue.
         *
         * @return rpcContextValue
         */
        public String getRpcContextValue() {
            return rpcContextValue;
        }

        /**
         * set rpcContextValue.
         *
         * @param rpcContextValue rpcContextValue
         */
        public void setRpcContextValue(final String rpcContextValue) {
            this.rpcContextValue = rpcContextValue;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            RpcContextHandleContent that = (RpcContextHandleContent) o;
            return Objects.equals(rpcContextType, that.rpcContextType)
                    && Objects.equals(rpcContextKey, that.rpcContextKey)
                    && Objects.equals(rpcContextValue, that.rpcContextValue);
        }

        @Override
        public int hashCode() {
            return Objects.hash(rpcContextType, rpcContextKey, rpcContextValue);
        }

        @Override
        public String toString() {
            return "RpcContextHandleContent{"
                    + "rpcContextType='" + rpcContextType + '\''
                    + ", rpcContextKey='" + rpcContextKey + '\''
                    + ", rpcContextValue='" + rpcContextValue + '\''
                    + '}';
        }
    }
}
