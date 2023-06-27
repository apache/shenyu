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

package org.apache.shenyu.plugin.grpc.intercept;

import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.ClientCall;
import io.grpc.ClientInterceptor;
import io.grpc.ForwardingClientCall;
import io.grpc.Metadata;
import io.grpc.MethodDescriptor;
import org.apache.shenyu.plugin.grpc.GrpcPlugin;

import java.util.Optional;

/**
 * Grpc context interceptor.
 */
public class ContextClientInterceptor implements ClientInterceptor {
    @Override
    public <R, P> ClientCall<R, P> interceptCall(final MethodDescriptor<R, P> methodDescriptor, final CallOptions callOptions, final Channel channel) {
        return new ForwardingClientCall.SimpleForwardingClientCall<R, P>(channel.newCall(methodDescriptor, callOptions)) {
            @Override
            public void start(final Listener<P> responseListener, final Metadata headers) {
                Optional.ofNullable(GrpcPlugin.RPC_CONTEXT_KEY.get()).ifPresent(map -> map.forEach((k, v) -> {
                    headers.put(Metadata.Key.of(k, Metadata.ASCII_STRING_MARSHALLER), v);
                }));
                super.start(responseListener, headers);
            }
        };
    }
}
