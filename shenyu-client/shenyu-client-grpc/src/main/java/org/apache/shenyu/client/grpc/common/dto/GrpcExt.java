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

package org.apache.shenyu.client.grpc.common.dto;

import io.grpc.MethodDescriptor;

import java.io.Serializable;

/**
 * The type Grpc ext.
 */
public class GrpcExt implements Serializable {

    private static final long serialVersionUID = 346053551613199101L;

    private String loadbalance;

    private Integer timeout;

    private MethodDescriptor.MethodType methodType;

    /**
     * constructor without parameter.
     */
    public GrpcExt() {
    }

    /**
     * constructor with all parameters.
     *
     * @param loadbalance loadbalance
     * @param timeout timeout
     * @param methodType methodType
     */
    public GrpcExt(final String loadbalance, final Integer timeout, final MethodDescriptor.MethodType methodType) {
        this.loadbalance = loadbalance;
        this.timeout = timeout;
        this.methodType = methodType;
    }

    /**
     * get loadbalance.
     *
     * @return loadbalance
     */
    public String getLoadbalance() {
        return loadbalance;
    }

    /**
     * set loadbalance.
     *
     * @param loadbalance loadbalance
     */
    public void setLoadbalance(final String loadbalance) {
        this.loadbalance = loadbalance;
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
     *
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    /**
     * get methodType.
     *
     * @return methodType
     */
    public MethodDescriptor.MethodType getMethodType() {
        return methodType;
    }

    /**
     * set methodType.
     *
     * @param methodType methodType
     */
    public void setMethodType(final MethodDescriptor.MethodType methodType) {
        this.methodType = methodType;
    }

    @Override
    public String toString() {
        return "GrpcExt{"
                + "loadbalance='" + loadbalance + '\''
                + ", timeout=" + timeout
                + ", methodType=" + methodType
                + '}';
    }

    /**
     * get Builder of GrpcExt.
     *
     * @return the Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * the Builder of GrpcExt.
     */
    public static final class Builder {

        private String loadbalance;

        private Integer timeout;

        private MethodDescriptor.MethodType methodType;

        /**
         * constructor without parameter.
         */
        private Builder() {
        }

        /**
         * set loadbalance.
         *
         * @param loadbalance loadbalance
         * @return Builder
         */
        public Builder loadbalance(final String loadbalance) {
            this.loadbalance = loadbalance;
            return this;
        }

        /**
         * set timeout.
         *
         * @param timeout timeout
         * @return Builder
         */
        public Builder timeout(final Integer timeout) {
            this.timeout = timeout;
            return this;
        }

        /**
         * set methodType.
         *
         * @param methodType methodType
         * @return Builder
         */
        public Builder methodType(final MethodDescriptor.MethodType methodType) {
            this.methodType = methodType;
            return this;
        }

        /**
         * build GrpcExt.
         *
         * @return GrpcExt
         */
        public GrpcExt build() {
            GrpcExt grpcExt = new GrpcExt();
            grpcExt.setLoadbalance(loadbalance);
            grpcExt.setTimeout(timeout);
            grpcExt.setMethodType(methodType);
            return grpcExt;
        }
    }
}
