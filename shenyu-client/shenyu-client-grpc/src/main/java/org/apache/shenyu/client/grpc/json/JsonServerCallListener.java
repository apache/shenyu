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

import com.google.protobuf.DynamicMessage;
import com.google.protobuf.Message;
import com.google.protobuf.util.JsonFormat;
import io.grpc.ForwardingServerCallListener;
import io.grpc.ServerCall;
import io.grpc.ServerCall.Listener;
import io.grpc.Status;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * handle request of json generic invoke.
 *
 * @param <R> generic request
 * @param <P> generic response
 */
public class JsonServerCallListener<R, P> extends ForwardingServerCallListener<R> {

    private static final Logger LOG = LoggerFactory.getLogger(JsonServerCallListener.class);

    private final Listener<R> delegate;

    private final ServerCall<R, P> call;

    public JsonServerCallListener(final Listener<R> delegate, final ServerCall<R, P> call) {
        this.delegate = delegate;
        this.call = call;
    }

    @Override
    protected Listener<R> delegate() {
        return delegate;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void onMessage(final R message) {
        Message.Builder builder;
        Class<?> t = JsonServerServiceInterceptor.getRequestClazzMap().get(call.getMethodDescriptor().getFullMethodName());
        try {
            builder = (Message.Builder) ReflectUtils.invokeMethod(t, "newBuilder");

            String reqData = JsonMessage.getDataFromDynamicMessage((DynamicMessage) message);
            JsonFormat.parser().ignoringUnknownFields().merge(reqData, builder);
            if (Objects.isNull(builder)) {
                throw new ShenyuException("build json response message is error, newBuilder method is null");
            }

            delegate.onMessage((R) builder.build());
        } catch (Exception e) {
            LOG.error("handle json generic request is error", e);
            throw Status.INTERNAL.withDescription(e.getMessage()).asRuntimeException();
        }
    }

    @Override
    public void onHalfClose() {
        super.onHalfClose();
    }

    @Override
    public void onCancel() {
        super.onCancel();
    }

    @Override
    public void onComplete() {
        super.onComplete();
    }

    @Override
    public void onReady() {
        super.onReady();
    }
}
