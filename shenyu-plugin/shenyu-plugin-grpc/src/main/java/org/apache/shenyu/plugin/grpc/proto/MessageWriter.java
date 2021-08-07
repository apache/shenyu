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
import com.google.protobuf.Message;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * MessageWriter.
 */
public final class MessageWriter<T extends Message> implements StreamObserver<T> {

    private static final Logger LOG = LoggerFactory.getLogger(MessageWriter.class);

    private final ShenyuGrpcResponse grpcResponse;

    private MessageWriter(final ShenyuGrpcResponse grpcResponse) {
        this.grpcResponse = grpcResponse;
    }

    /**
     * New instance.
     *
     * @param results  results
     * @param <T>      t
     * @return message message
     */
    public static <T extends Message> MessageWriter<T> newInstance(final ShenyuGrpcResponse results) {
        return new MessageWriter<>(results);
    }

    @Override
    public void onNext(final T value) {
        String respData = JsonMessage.getDataFromDynamicMessage((DynamicMessage) value);
        grpcResponse.getResults().add(respData);
    }

    @Override
    public void onError(final Throwable t) {
        LOG.error("Messages write occur errors", t);
    }

    @Override
    public void onCompleted() {
        LOG.info("Messages write complete");
    }
}
