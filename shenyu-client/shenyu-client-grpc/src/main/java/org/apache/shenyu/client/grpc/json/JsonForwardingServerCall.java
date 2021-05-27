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

package org.apache.shenyu.client.grpc.json;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.MessageOrBuilder;
import com.google.protobuf.util.JsonFormat;
import io.grpc.Attributes;
import io.grpc.MethodDescriptor;
import io.grpc.Status;
import io.grpc.ServerCall;
import io.grpc.Metadata;

import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.protocol.grpc.message.JsonReply;


/**
 * Handle response of json generic service.
 *
 * @param <R> request message
 * @param <P> response message
 */
@Slf4j
public class JsonForwardingServerCall<R, P> extends ServerCall<R, P> {

    private final ServerCall<P, P> call;

    public JsonForwardingServerCall(final ServerCall<P, P> call) {
        this.call = call;
    }

    protected ServerCall<P, P> delegate() {
        return call;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void sendMessage(final P message) {
        String jsonFormat;
        JsonReply rep;
        try {
            if (message == null) {
                delegate().sendMessage(null);
                return;
            }

            jsonFormat = JsonFormat.printer().includingDefaultValueFields().preservingProtoFieldNames()
                    .print((MessageOrBuilder) message);

            rep = JsonReply.newBuilder().setMessage(jsonFormat).build();
            log.debug("begin send json response: {}", jsonFormat);
            delegate().sendMessage((P) rep);
        } catch (InvalidProtocolBufferException e) {
            log.error("handle json message is error", e);
            throw Status.INTERNAL.withDescription(e.getMessage()).asRuntimeException();
        }
    }

    @Override
    public void request(final int numMessages) {
        delegate().request(numMessages);
    }

    @Override
    public void sendHeaders(final Metadata headers) {
        delegate().sendHeaders(headers);
    }

    @Override
    public boolean isReady() {
        return delegate().isReady();
    }

    @Override
    public void close(final Status status, final Metadata trailers) {
        delegate().close(status, trailers);
    }

    @Override
    public boolean isCancelled() {
        return delegate().isCancelled();
    }

    @Override
    public void setMessageCompression(final boolean enabled) {
        delegate().setMessageCompression(enabled);
    }

    @Override
    public void setCompression(final String compressor) {
        delegate().setCompression(compressor);
    }

    @Override
    public Attributes getAttributes() {
        return delegate().getAttributes();
    }

    @SuppressWarnings("unchecked")
    @Override
    public MethodDescriptor<R, P> getMethodDescriptor() {
        return (MethodDescriptor<R, P>) delegate().getMethodDescriptor();
    }

}
