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

package org.apache.shenyu.client.tars.common.dto;

import org.apache.commons.lang3.tuple.Pair;

import java.util.List;

/**
 * The type Tars rpc ext.
 */
public class TarsRpcExt {

    /**
     * in order to be compatible with the old version,
     * we can't change the type of this field.
     */
    private List<RpcExt> methodInfo;

    /**
     * constructor without params.
     */
    public TarsRpcExt() {
    }

    /**
     * constructor with all params.
     *
     * @param methodInfo methodInfo
     */
    public TarsRpcExt(final List<RpcExt> methodInfo) {
        this.methodInfo = methodInfo;
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
        return "TarsRpcExt{"
                + "methodInfo=" + methodInfo
                + '}';
    }

    /**
     * The type Rpc ext.
     */
    public static class RpcExt {
        
        private String methodName;
        
        private List<Pair<String, String>> params;
        
        private String returnType;

        /**
         * constructor without params.
         */
        public RpcExt() {
        }

        /**
         * constructor with params.
         *
         * @param methodName methodName
         * @param params params
         * @param returnType returnType
         */
        public RpcExt(final String methodName, final List<Pair<String, String>> params, final String returnType) {
            this.methodName = methodName;
            this.params = params;
            this.returnType = returnType;
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
         * get params.
         *
         * @return params.
         */
        public List<Pair<String, String>> getParams() {
            return params;
        }

        /**
         * set params.
         *
         * @param params params
         */
        public void setParams(final List<Pair<String, String>> params) {
            this.params = params;
        }

        /**
         * get returnType.
         *
         * @return returnType
         */
        public String getReturnType() {
            return returnType;
        }

        /**
         * set returnType.
         *
         * @param returnType returnType
         */
        public void setReturnType(final String returnType) {
            this.returnType = returnType;
        }

        @Override
        public String toString() {
            return "RpcExt{"
                    + "methodName='" + methodName + '\''
                    + ", params=" + params
                    + ", returnType='" + returnType + '\''
                    + '}';
        }
    }
}
