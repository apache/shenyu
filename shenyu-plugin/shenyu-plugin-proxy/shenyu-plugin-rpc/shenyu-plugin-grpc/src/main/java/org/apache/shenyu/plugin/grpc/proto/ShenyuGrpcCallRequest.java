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

package org.apache.shenyu.plugin.grpc.proto;

import com.google.protobuf.DynamicMessage;
import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;

import java.util.List;

/**
 * ShenyuGrpcCallRequest.
 */
public class ShenyuGrpcCallRequest {

    private Channel channel;

    private CallOptions callOptions;

    private List<DynamicMessage> requests;

    private MethodDescriptor<DynamicMessage, DynamicMessage> methodDescriptor;

    private StreamObserver<DynamicMessage> responseObserver;

    /**
     * Gets channel.
     *
     * @return the channel
     */
    public Channel getChannel() {
        return channel;
    }

    /**
     * Sets channel.
     *
     * @param channel the channel
     */
    public void setChannel(final Channel channel) {
        this.channel = channel;
    }

    /**
     * Gets call options.
     *
     * @return the call options
     */
    public CallOptions getCallOptions() {
        return callOptions;
    }

    /**
     * Sets call options.
     *
     * @param callOptions the call options
     */
    public void setCallOptions(final CallOptions callOptions) {
        this.callOptions = callOptions;
    }

    /**
     * Gets requests.
     *
     * @return the requests
     */
    public List<DynamicMessage> getRequests() {
        return requests;
    }

    /**
     * Sets requests.
     *
     * @param requests the requests
     */
    public void setRequests(final List<DynamicMessage> requests) {
        this.requests = requests;
    }

    /**
     * Gets method descriptor.
     *
     * @return the method descriptor
     */
    public MethodDescriptor<DynamicMessage, DynamicMessage> getMethodDescriptor() {
        return methodDescriptor;
    }

    /**
     * Sets method descriptor.
     *
     * @param methodDescriptor the method descriptor
     */
    public void setMethodDescriptor(final MethodDescriptor<DynamicMessage, DynamicMessage> methodDescriptor) {
        this.methodDescriptor = methodDescriptor;
    }

    /**
     * Gets response observer.
     *
     * @return the response observer
     */
    public StreamObserver<DynamicMessage> getResponseObserver() {
        return responseObserver;
    }

    /**
     * Sets response observer.
     *
     * @param responseObserver the response observer
     */
    public void setResponseObserver(final StreamObserver<DynamicMessage> responseObserver) {
        this.responseObserver = responseObserver;
    }
}
