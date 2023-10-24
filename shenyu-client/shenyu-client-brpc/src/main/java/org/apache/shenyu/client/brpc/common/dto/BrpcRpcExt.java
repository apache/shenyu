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

package org.apache.shenyu.client.brpc.common.dto;

import org.apache.commons.lang3.tuple.Pair;

import java.util.List;

/**
 * Brpc rpc ext.
 */
public class BrpcRpcExt {

    /**
     * in order to be compatible with the old version,
     * we can't change the type of this field.
     */
    private List<RpcExt> methodInfo;
    
    private String host;
    
    private Integer port;

    /**
     * constructor without params.
     */
    public BrpcRpcExt() {
    }

    /**
     * constructor with all params.
     *
     * @param methodInfo methodInfo
     * @param host host
     * @param port port
     */
    public BrpcRpcExt(final List<RpcExt> methodInfo, final String host, final Integer port) {
        this.methodInfo = methodInfo;
        this.host = host;
        this.port = port;
    }
    
    /**
     * get port.
     *
     * @return port
     */
    public Integer getPort() {
        return port;
    }
    
    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }
    
    /**
     * get host.
     *
     * @return host
     */
    public String getHost() {
        return host;
    }
    
    /**
     * set host.
     *
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }
    
    /**
     * get methodInfo.
     *
     * @return methodInfo
     */
    public List<RpcExt> getMethodInfo() {
        return methodInfo;
    }

    /**
     * set methodInfo.
     *
     * @param methodInfo methodInfo
     */
    public void setMethodInfo(final List<RpcExt> methodInfo) {
        this.methodInfo = methodInfo;
    }

    @Override
    public String toString() {
        return "BrpcRpcExt{"
                + "methodInfo=" + methodInfo
                + '}';
    }

    /**
     * The type Rpc ext.
     */
    public static class RpcExt {
        
        private String methodName;
        
        private List<Pair<String, String>> paramTypes;

        /**
         * constructor without params.
         */
        public RpcExt() {
        }

        /**
         * constructor with params.
         * @param methodName methodName
         * @param paramTypes params
         */
        public RpcExt(final String methodName, final List<Pair<String, String>> paramTypes) {
            this.methodName = methodName;
            this.paramTypes = paramTypes;
        }

        /**
         * get methodName.
         *
         * @return methodName
         */
        public String getMethodName() {
            return methodName;
        }

        /**
         * set methodName.
         *
         * @param methodName methodName
         */
        public void setMethodName(final String methodName) {
            this.methodName = methodName;
        }

        /**
         * get param types.
         *
         * @return param types.
         */
        public List<Pair<String, String>> getParamTypes() {
            return paramTypes;
        }

        /**
         * set param types.
         *
         * @param paramTypes param types
         */
        public void setParamTypes(final List<Pair<String, String>> paramTypes) {
            this.paramTypes = paramTypes;
        }

        @Override
        public String toString() {
            return "RpcExt{"
                    + "methodName='" + methodName + '\''
                    + ", paramTypes=" + paramTypes
                    + '}';
        }
    }
}
