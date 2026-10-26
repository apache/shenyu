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

package org.apache.shenyu.plugin.grpc.cache;

import io.grpc.MethodDescriptor;

import java.util.Objects;

/**
 * The GrpcExtInfo.
 */
public class GrpcExtInfo {

    private Integer timeout = 5000;

    private MethodDescriptor.MethodType methodType = MethodDescriptor.MethodType.UNARY;

    /**
     * Instantiates a new Grpc ext info.
     */
    public GrpcExtInfo() {
    }

    /**
     * Instantiates a new Grpc ext info.
     *
     * @param timeout timeout
     * @param methodType methodType
     */
    public GrpcExtInfo(final Integer timeout, final MethodDescriptor.MethodType methodType) {
        this.timeout = timeout;
        this.methodType = methodType;
    }

    /**
     * Gets timeout.
     *
     * @return timeout
     */
    public Integer getTimeout() {
        return timeout;
    }

    /**
     * Sets timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final Integer timeout) {
        this.timeout = timeout;
    }

    /**
     * Gets methodType.
     *
     * @return methodType
     */
    public MethodDescriptor.MethodType getMethodType() {
        return methodType;
    }

    /**
     * Sets methodType.
     *
     * @param methodType methodType
     */
    public void setMethodType(final MethodDescriptor.MethodType methodType) {
        this.methodType = methodType;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GrpcExtInfo that = (GrpcExtInfo) o;
        return Objects.equals(timeout, that.timeout) && methodType == that.methodType;
    }

    @Override
    public int hashCode() {
        return Objects.hash(timeout, methodType);
    }
}
