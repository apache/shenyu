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

package org.apache.shenyu.client.motan.common.dto;

import org.apache.commons.lang3.tuple.Pair;

import java.util.List;

/**
 * Motan rpc ext.
 */
public class MotanRpcExt {

    /**
     * in order to be compatible with the old version,
     * we can't change the type of this field.
     */
    private List<RpcExt> methodInfo;

    private String group;

    private Integer timeout;

    /**
     * constructor without params.
     */
    public MotanRpcExt() {
    }

    /**
     * constructor with all params.
     *
     * @param methodInfo methodInfo
     * @param group group
     * @param timeout timeout
     */
    public MotanRpcExt(final List<RpcExt> methodInfo, final String group, final Integer timeout) {
        this.methodInfo = methodInfo;
        this.group = group;
        this.timeout = timeout;
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

    /**
     * get group.
     *
     * @return group
     */
    public String getGroup() {
        return group;
    }

    /**
     * set group.
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * get timeout.
     *
     * @return timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * set timeout.
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    @Override
    public String toString() {
        return "MotanRpcExt{"
                + "methodInfo=" + methodInfo
                + ", group='" + group + '\''
                + ", timeout='" + timeout + '\''
                + '}';
    }

    /**
     * The type Rpc ext.
     */
    public static class RpcExt {

        private String methodName;

        private List<Pair<String, String>> params;

        /**
         * constructor without params.
         */
        public RpcExt() {
        }

        /**
         * constructor with all params.
         *
         * @param methodName methodName
         * @param params params
         */
        public RpcExt(final String methodName, final List<Pair<String, String>> params) {
            this.methodName = methodName;
            this.params = params;
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
         * @return params
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

        @Override
        public String toString() {
            return "RpcExt{"
                    + "methodName='" + methodName + '\''
                    + ", params=" + params
                    + '}';
        }
    }
}
